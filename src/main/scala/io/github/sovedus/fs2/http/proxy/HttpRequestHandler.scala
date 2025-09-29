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

import io.github.sovedus.fs2.http.proxy.parsers.ResponseParser

import cats.data.OptionT
import cats.effect.{Async, Resource}
import cats.effect.implicits.{effectResourceOps, genSpawnOps}
import cats.syntax.all.*

import org.http4s.{Charset, Headers, MediaType, Request, Response, Status}
import org.http4s.headers.{`Content-Length`, `Content-Type`}
import org.typelevel.log4cats.Logger

import java.nio.channels.ClosedChannelException

import com.comcast.ip4s.{Host, Port, SocketAddress}
import fs2.io.net.Network

trait HttpRequestHandler[F[_]] {
  def handleRequest(req: Request[F]): Resource[F, Response[F]]
}

object HttpRequestHandler {

  private val BUFFER_SIZE = 16 * 1024

  private class DefaultHttpRequestHandler[F[_]: Async: Network](logger: Logger[F])
      extends HttpRequestHandler[F] {

    private val invalidAddressResponse =
      errResponse(Status.BadRequest, "Could not extract host and/or port from request URI")

    private val exceptionResponse =
      errResponse(Status.InternalServerError, _)

    override def handleRequest(req: Request[F]): Resource[F, Response[F]] = {
      val socketAddressOpt = for {
        host <- OptionT
          .fromOption(req.uri.host)
          .subflatMap(host => Host.fromString(host.renderString))
        port <- OptionT.fromOption(Port.fromInt(req.uri.port.getOrElse(80)))
      } yield SocketAddress(host, port)

      Resource
        .eval(socketAddressOpt.value)
        .flatMap {
          case Some(address) => sendRequest(address, req)
          case None => Resource.pure(invalidAddressResponse)
        }
        .recoverWith(ex =>
          logger
            .error(ex)("Handle request error")
            .toResource
            .as(exceptionResponse(ex.getMessage))
        )
    }

    private def sendRequest(
        address: SocketAddress[Host],
        request: Request[F]
    ): Resource[F, Response[F]] = {
      Network[F].client(address).evalMap { socket =>
        for {
          _ <- Encoder
            .encode(request.withUri(request.uri.toOriginForm))
            .through(socket.writes)
            .compile
            .drain
            .start
            .void
          read = socket.read(BUFFER_SIZE).recoverWith {
            case _: ClosedChannelException => Async[F].pure(None)
          }
          resp <- ResponseParser.parse(read)
        } yield resp
      }
    }

    private def errResponse(status: Status, body: String): Response[F] = {
      val bytes = body.getBytes(Charset.`UTF-8`.nioCharset)
      Response[F](
        status = status,
        headers = Headers(
          `Content-Type`(MediaType.text.plain, Charset.`UTF-8`),
          `Content-Length`.unsafeFromLong(bytes.length.toLong)
        ),
        body = fs2.Stream.emits(bytes)
      )
    }
  }

  def default[F[_]: Async: Network](logger: Logger[F]): HttpRequestHandler[F] =
    new DefaultHttpRequestHandler(logger)
}
