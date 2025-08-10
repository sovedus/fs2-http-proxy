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

import cats.effect.{Async, Deferred, Resource}
import cats.effect.implicits.genSpawnOps
import cats.syntax.all.*

import org.typelevel.log4cats.{Logger, LoggerFactory}

import scala.concurrent.duration.Duration

import com.comcast.ip4s.{Host, IpAddress, Port, SocketAddress}
import fs2.io.net.{Network, SocketOption}

final class HttpProxyServerBuilder[F[_]: Async: Network] private (
    val host: Option[Host],
    val port: Option[Port],
    val maxConnections: Int,
    private val receiveBufferSize: Int,
    private val shutdownTimeout: Duration,
    private val connectRequestHandlerOpt: Option[ConnectRequestHandler[F]],
    private val httpRequestHandlerOpt: Option[HttpRequestHandler[F]],
    private val socketOptions: List[SocketOption],
    private val logger: Logger[F],
    private val errorHandler: PartialFunction[Throwable, F[Unit]]
) {

  def withHost(host: Host): HttpProxyServerBuilder[F] = copy(host = Option(host))

  def withPort(port: Port): HttpProxyServerBuilder[F] = copy(port = Option(port))

  def withMaxConnections(maxConnections: Int): HttpProxyServerBuilder[F] = {
    assert(maxConnections > 0, "maxConnections must be greater than zero")
    copy(maxConnections = maxConnections)
  }

  def withReceiveBufferSize(receiveBufferSize: Int): HttpProxyServerBuilder[F] = {
    assert(maxConnections > 0, "receiveBufferSize must be greater than zero")
    copy(receiveBufferSize = receiveBufferSize)
  }

  def withShutdownTimeout(shutdownTimeout: Duration): HttpProxyServerBuilder[F] =
    copy(shutdownTimeout = shutdownTimeout)

  def withConnectRequestHandler(handler: ConnectRequestHandler[F]): HttpProxyServerBuilder[F] =
    copy(connectRequestHandlerOpt = Option(handler))

  def withHttpRequestHandler(handler: HttpRequestHandler[F]): HttpProxyServerBuilder[F] =
    copy(httpRequestHandlerOpt = Option(handler))

  def withSocketOptions(socketOptions: List[SocketOption]): HttpProxyServerBuilder[F] =
    copy(socketOptions = socketOptions)

  def withLogger(logger: Logger[F]): HttpProxyServerBuilder[F] = copy(logger = logger)

  def withErrorHandler(
      errorHandler: PartialFunction[Throwable, F[Unit]]
  ): HttpProxyServerBuilder[F] =
    copy(errorHandler = errorHandler)

  def build: Resource[F, Server] =
    for {
      sg <- Resource.pure(Network[F])
      shutdown <- Resource.eval(Shutdown(shutdownTimeout))
      ready <- Resource.eval(Deferred[F, Either[Throwable, SocketAddress[IpAddress]]])
      connectRequestHandler = connectRequestHandlerOpt.getOrElse(
        ConnectRequestHandler.default(logger))
      httpRequestHandler = httpRequestHandlerOpt.getOrElse(
        HttpRequestHandler.default(logger)
      )
      _ <- HttpProxyServer
        .run(
          sg,
          shutdown,
          host,
          port,
          maxConnections,
          receiveBufferSize,
          socketOptions,
          logger,
          errorHandler,
          connectRequestHandler,
          httpRequestHandler,
          ready
        )
        .background
      _ <- Resource.onFinalize(shutdown.shutdown)
      address <- Resource.eval(ready.get.rethrow)
      _ <- Resource.eval(logger.info(s"HTTP proxy server bound to address: $address"))
    } yield Server(address.host, address.port)

  private def copy(
      host: Option[Host] = this.host,
      port: Option[Port] = this.port,
      maxConnections: Int = this.maxConnections,
      receiveBufferSize: Int = this.receiveBufferSize,
      shutdownTimeout: Duration = this.shutdownTimeout,
      connectRequestHandlerOpt: Option[ConnectRequestHandler[F]] =
        this.connectRequestHandlerOpt,
      httpRequestHandlerOpt: Option[HttpRequestHandler[F]] = this.httpRequestHandlerOpt,
      socketOptions: List[SocketOption] = this.socketOptions,
      logger: Logger[F] = this.logger,
      errorHandler: PartialFunction[Throwable, F[Unit]] = this.errorHandler
  ): HttpProxyServerBuilder[F] =
    new HttpProxyServerBuilder[F](
      host = host,
      port = port,
      maxConnections = maxConnections,
      receiveBufferSize = receiveBufferSize,
      shutdownTimeout = shutdownTimeout,
      connectRequestHandlerOpt = connectRequestHandlerOpt,
      httpRequestHandlerOpt = httpRequestHandlerOpt,
      socketOptions = socketOptions,
      logger = logger,
      errorHandler = errorHandler
    )
}

object HttpProxyServerBuilder {
  def default[F[_]: Async: Network: LoggerFactory]: HttpProxyServerBuilder[F] =
    new HttpProxyServerBuilder(
      host = None,
      port = None,
      maxConnections = Defaults.MAX_CONNECTIONS,
      receiveBufferSize = Defaults.RECEIVE_BUFFER_SIZE,
      shutdownTimeout = Defaults.SHUTDOWN_TIMEOUT,
      connectRequestHandlerOpt = None,
      httpRequestHandlerOpt = None,
      socketOptions = Nil,
      logger = LoggerFactory[F].getLogger,
      errorHandler = Defaults.errorHandler
    )
}
