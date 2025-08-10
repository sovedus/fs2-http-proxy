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

import org.http4s.{Headers, Response, Status}

import fs2.{Pipe, Stream}

sealed trait ConnectAction[F[_]] extends Serializable with Product

object ConnectAction {
  case class Accept[F[_]] private (headers: Headers, tunnel: Pipe[F, Byte, Byte])
      extends ConnectAction[F]

  case class Reject[F[_]] private (
      resp: Option[Response[F]]
  ) extends ConnectAction[F]

  def accept[F[_]](headers: Headers)(tunnel: Pipe[F, Byte, Byte]): Accept[F] =
    Accept(headers, tunnel)

  def accept[F[_]](tunnel: Pipe[F, Byte, Byte]): Accept[F] = Accept(Headers.empty, tunnel)

  def reject[F[_]]: Reject[F] = Reject(None)

  def reject[F[_]](status: Status, headers: Headers, body: Stream[F, Byte]): Reject[F] =
    Reject(Some(Response[F](status = status, headers = headers, body = body)))

  def reject[F[_]](status: Status, headers: Headers): Reject[F] =
    reject(status, headers, Stream.empty.covary[F])

  def reject[F[_]](status: Status, body: Stream[F, Byte]): Reject[F] =
    reject(status, Headers.empty, body)

  def reject[F[_]](resp: Response[F]): Reject[F] = Reject(Some(resp))

}
