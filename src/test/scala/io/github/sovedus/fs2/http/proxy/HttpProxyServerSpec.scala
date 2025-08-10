package io.github.sovedus.fs2.http.proxy

import io.github.sovedus.fs2.http.proxy.parsers.ResponseParser

import cats.data.OptionT
import cats.effect.{IO, Resource}

import org.http4s.{Headers, Method, Request, Response, Status}
import org.http4s.syntax.literals.*

import com.comcast.ip4s.{Host, SocketAddress}
import fs2.io.net.Network

class HttpProxyServerSpec extends BaseSpec {

  "HttpProxyServer" should "establish a tunnel for CONNECT requests and return the passed data" in {
    val echoConnectRequestHandler = new ConnectRequestHandler[IO] {
      override def handleRequest(req: Request[IO]): IO[ConnectAction[IO]] =
        IO(ConnectAction.accept(stream => stream))
    }

    createServer(echoConnectRequestHandler, null)
      .flatMap(address => Network[IO].client(address))
      .use { client =>
        for {
          _ <- Encoder
            .encode(Request[IO](Method.CONNECT, uri"localhost:1234"))
            .through(client.writes)
            .compile
            .drain
          resp <- ResponseParser.parse(client.read(1024))
          _ = resp.status shouldBe Status.Ok
          _ = resp.headers shouldBe Headers.empty
          _ <- fs2
            .Stream
            .emit("foo")
            .through(fs2.text.utf8.encode)
            .through(client.writes)
            .compile
            .drain
          _ <- OptionT(client.read(10))
            .map(c => new String(c.toArray))
            .map(_ shouldBe "foo")
            .getOrElse(fail("Empty server response"))
        } yield {}
      }
  }

  it should "reject connection without response" in {
    val rejectConnectRequestHandler = new ConnectRequestHandler[IO] {
      override def handleRequest(req: Request[IO]): IO[ConnectAction[IO]] =
        IO(ConnectAction.reject[IO])
    }

    createServer(rejectConnectRequestHandler, null)
      .flatMap(address => Network[IO].client(address))
      .use { client =>
        for {
          _ <- Encoder
            .encode(Request[IO](Method.CONNECT, uri"localhost:1234"))
            .through(client.writes)
            .compile
            .drain
          _ <- OptionT(client.read(100))
            .map(c => new String(c.toArray))
            .map(s => fail(s"Unexpected server response: $s"))
            .getOrElse(succeed)
        } yield {}
      }
  }

  it should "reject connection with response" in {
    val htmlResponse = "<h1>BadGateway</h1>"

    val rejectConnectRequestHandler = new ConnectRequestHandler[IO] {
      override def handleRequest(req: Request[IO]): IO[ConnectAction[IO]] =
        IO {
          val stream = fs2.Stream.emit(htmlResponse).through(fs2.text.utf8.encode)
          ConnectAction.reject[IO](Response[IO](Status.BadGateway, body = stream))
        }
    }

    createServer(rejectConnectRequestHandler, null)
      .flatMap(address => Network[IO].client(address))
      .use { client =>
        for {
          _ <- Encoder
            .encode(Request[IO](Method.CONNECT, uri"localhost:1234"))
            .through(client.writes)
            .compile
            .drain
          resp <- ResponseParser.parse(client.read(1024))
          _ = resp.status shouldBe Status.BadGateway
          _ = resp.headers shouldBe Headers.empty
          respContent <- resp.body.through(fs2.text.utf8.decode).compile.string
          _ = respContent shouldBe htmlResponse
        } yield {}
      }
  }

  it should "forward the request to the destination and return the response" in {
    val simpleHttpRequestHandler = new HttpRequestHandler[IO] {
      override def handleRequest(req: Request[IO]): IO[Response[IO]] =
        IO(Response[IO](Status.NoContent))
    }

    createServer(null, simpleHttpRequestHandler)
      .flatMap(address => Network[IO].client(address))
      .use { client =>
        for {
          _ <- Encoder
            .encode(Request[IO](Method.GET, uri"/"))
            .through(client.writes)
            .compile
            .drain
          resp <- ResponseParser.parse(client.read(1024))
          _ = resp.status shouldBe Status.NoContent
          _ = resp.headers shouldBe Headers.empty
        } yield {}
      }

  }

  private def createServer(
      connectRequestHandler: ConnectRequestHandler[IO],
      httpRequestHandler: HttpRequestHandler[IO]
  ): Resource[IO, SocketAddress[Host]] =
    HttpProxyServerBuilder
      .default[IO]
      .withConnectRequestHandler(connectRequestHandler)
      .withHttpRequestHandler(httpRequestHandler)
      .build
      .map(s => SocketAddress(s.host, s.port))
}
