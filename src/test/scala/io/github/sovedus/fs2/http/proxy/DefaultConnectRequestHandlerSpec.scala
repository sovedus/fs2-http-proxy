/*
 * Copyright 2025 Sovedus
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.github.sovedus.fs2.http.proxy

import cats.effect.{IO, Resource}

import org.http4s.{Header, Method, Request, Uri}
import org.scalatest.Assertion
import org.typelevel.ci.CIStringSyntax
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.DurationInt

import com.comcast.ip4s.{Host, SocketAddress}
import fs2.io.net.Network

class DefaultConnectRequestHandlerSpec extends BaseSpec {

  private val logger = Slf4jLogger.getLogger[IO]
  private val handler: ConnectRequestHandler[IO] = ConnectRequestHandler.default[IO](logger)

  "DefaultConnectRequestHandler" should "successfully forward request and return response when used hostname" in
    serverTest(handler) { address =>
      Request[IO]()
        .withMethod(Method.CONNECT)
        .withUri(Uri.requestTarget(s"localhost:${address.port.value}").value)
        .withHeaders(Header.Raw(ci"Host", s"localhost:${address.port.value}"))
        .withHeaders(Header.Raw(ci"User-Agent", "curl/8.15.0"))
    }

  it should "successfully forward request and return response when used IpV4" in
    serverTest(handler) { address =>
      Request[IO]()
        .withMethod(Method.CONNECT)
        .withUri(Uri.requestTarget(s"127.0.0.1:${address.port.value}").value)
        .withHeaders(Header.Raw(ci"Host", s"127.0.0.1:${address.port.value}"))
        .withHeaders(Header.Raw(ci"User-Agent", "curl/8.15.0"))
    }

  it should "successfully forward request and return response when used IpV6" in
    serverTest(handler) { address =>
      Request[IO]()
        .withMethod(Method.CONNECT)
        .withUri(Uri.requestTarget(s"[::]:${address.port.value}").value)
        .withHeaders(Header.Raw(ci"Host", s"[::]:${address.port.value}"))
        .withHeaders(Header.Raw(ci"User-Agent", "curl/8.15.0"))
    }

  private def serverTest(
      handler: ConnectRequestHandler[IO]
  )(request: SocketAddress[Host] => Request[IO]): IO[Assertion] = {
    val res = for {
      (address, sockets) <- Network[IO].serverResource()
      _ <- sockets
        .map(socket => socket.reads.through(socket.writes))
        .parJoin(1)
        .compile
        .drain
        .background
      action <- handler.handleRequest(request(address))
      result <- Resource.eval {
        action match {
          case ConnectAction.Accept(_, tunnel) =>
            fs2.Stream
              .emit("ping")
              .through(fs2.text.utf8.encode)
              .through(tunnel)
              .through(fs2.text.utf8.decode)
              .interruptAfter(100.millis)
              .compile
              .string
          case ConnectAction.Reject(_) => IO(fail("Handler return reject action"))
        }
      }
    } yield result

    res.use(r => IO(r shouldBe "ping"))
  }
}
