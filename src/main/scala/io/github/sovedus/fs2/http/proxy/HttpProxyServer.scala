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

import io.github.sovedus.fs2.http.proxy.parsers.*

import cats.effect.{Async, Deferred}
import cats.syntax.all.*

import org.http4s.{Method, Request, Response, Status}
import org.typelevel.log4cats.Logger

import com.comcast.ip4s.{Host, IpAddress, Port, SocketAddress}
import fs2.*
import fs2.io.net.{Socket, SocketGroup, SocketOption}

object HttpProxyServer {
  def run[F[_]: Async](
      sg: SocketGroup[F],
      shutdown: Shutdown[F],
      host: Option[Host],
      port: Option[Port],
      maxConnections: Int,
      receiveBufferSize: Int,
      socketOptions: List[SocketOption],
      logger: Logger[F],
      errorHandler: PartialFunction[Throwable, F[Unit]],
      connectRequestHandler: ConnectRequestHandler[F],
      httpRequestHandler: HttpRequestHandler[F],
      ready: Deferred[F, Either[Throwable, SocketAddress[IpAddress]]]
  ): F[Unit] =
    Stream
      .resource(sg.serverResource(host, port, socketOptions))
      .attempt
      .evalTap(e => ready.complete(e.map(_._1)))
      .rethrow
      .flatMap(_._2)
      .map { connection =>
        val read = connection.read(receiveBufferSize)

        val stream = for {
          _ <- shutdown.trackConnection
          req <- Stream.eval(RequestParser.parse(read))
          _ <- runReqHandler(req, connectRequestHandler, httpRequestHandler, connection, read)
        } yield {}

        def finalErrorHandler(t: Throwable): F[Unit] =
          errorHandler.applyOrElse(
            t,
            (t: Throwable) => logger.error(t)("Handler failed with exception")
          )

        stream.handleErrorWith {
          case HttpProxyServerException.EmptyStream() =>
            Stream.unit
          case ex => Stream.eval(finalErrorHandler(ex))
        }
      }
      .parJoin(maxConnections)
      .compile
      .drain

  private def runReqHandler[F[_]: Async](
      req: Request[F],
      connectRequestHandler: ConnectRequestHandler[F],
      httpRequestHandler: HttpRequestHandler[F],
      connection: Socket[F],
      read: F[Option[Chunk[Byte]]]
  ): Stream[F, Unit] =
    req.method match {
      case Method.CONNECT =>
        runConnectReqHandler(req, connectRequestHandler, read, connection)
      case _ =>
        runHttpReqHandler(req, httpRequestHandler, connection)
    }

  private def runConnectReqHandler[F[_]: Async](
      req: Request[F],
      connectRequestHandler: ConnectRequestHandler[F],
      read: F[Option[Chunk[Byte]]],
      connection: Socket[F]
  ): Stream[F, Unit] =
    for {
      action <- Stream.eval(connectRequestHandler.handleRequest(req))
      stream = tunnelStream(read)
      _ <- action match {
        case a: ConnectAction.Accept[F] =>
          Stream.eval(write(connection, Response[F](Status.Ok, headers = a.headers))) >>
            a.tunnel(stream).through(connection.writes).void
        case a: ConnectAction.Reject[F] =>
          Stream.emit(a.resp).unNone.foreach(write(connection, _))
      }
    } yield {}

  private def runHttpReqHandler[F[_]: Async](
      req: Request[F],
      httpRequestHandler: HttpRequestHandler[F],
      connection: Socket[F]
  ): Stream[F, Unit] =
    Stream
      .eval(httpRequestHandler.handleRequest(req))
      .flatMap(Encoder.encode[F])
      .through(connection.writes)
      .void

  private def tunnelStream[F[_]](read: F[Option[Chunk[Byte]]]): Stream[F, Byte] = {
    val bytesPull = Pull.eval(read)

    def go(): Pull[F, Byte, Unit] =
      bytesPull.flatMap {
        case Some(chunk) => Pull.output(chunk) >> go()
        case None => Pull.done
      }

    go().stream
  }

  private def write[F[_]: Async](
      connection: Socket[F],
      resp: Response[F]
  ): F[Unit] =
    Encoder.encode(resp).chunks.evalMap(connection.write).compile.drain

}
