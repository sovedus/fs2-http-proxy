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

import cats.effect.Async
import cats.syntax.all.*

import org.http4s.{ParseFailure, Request, Uri}
import org.typelevel.log4cats.Logger

import com.comcast.ip4s.{Host, Port, SocketAddress}
import fs2.io.net.Network

trait ConnectRequestHandler[F[_]] {
  def handleRequest(req: Request[F]): F[ConnectAction[F]]
}

object ConnectRequestHandler {
  private class DefaultConnectRequestHandler[F[_]: Async: Network](logger: Logger[F])
      extends ConnectRequestHandler[F] {

    override def handleRequest(req: Request[F]): F[ConnectAction[F]] =
      Async[F]
        .delay(extractHostAndPort(req.uri))
        .map { case (host, port) => (host, port.getOrElse(Port.fromInt(443).get)) }
        .map { case (host, port) => SocketAddress(host, port) }
        .attemptT
        .map(acceptAction)
        .valueOrF(ex =>
          logger.error(ex)("Handle connect request error").as(ConnectAction.reject[F]))

    private def acceptAction(address: SocketAddress[Host]): ConnectAction.Accept[F] =
      ConnectAction.accept { stream =>
        fs2.Stream.resource(Network[F].client(address)).flatMap { socket =>
          val writeS = stream.chunks.foreach(chunk => socket.write(chunk))

          socket.reads.concurrently(writeS)
        }
      }

    private def extractHostAndPort(uri: Uri): (Host, Option[Port]) =
      if (uri.host.isEmpty) {
        val address = uri.renderString.stripPrefix("//")
        val lastColonIdx = address.lastIndexOf(':')

        val tuple = address.splitAt(lastColonIdx)
        val (rawHost, rawPort) = (tuple._1, tuple._2.drop(1))

        val host = Host
          .fromString(rawHost)
          .getOrElse(throw new IllegalArgumentException("Request URI not contains host"))

        val port = Port.fromString(rawPort)

        (host, port)
      } else {
        val host = uri
          .host
          .map(_.renderString.stripPrefix("[").stripSuffix("]"))
          .flatMap(Host.fromString)
          .getOrElse(throw new ParseFailure("Fail parse request URI", ""))

        val port = uri.port.flatMap(Port.fromInt)

        (host, port)
      }
  }

  def default[F[_]: Async: Network](logger: Logger[F]): ConnectRequestHandler[F] =
    new DefaultConnectRequestHandler(logger)

}
