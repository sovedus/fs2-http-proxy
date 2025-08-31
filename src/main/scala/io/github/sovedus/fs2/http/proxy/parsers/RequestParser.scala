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

package io.github.sovedus.fs2.http.proxy.parsers

import io.github.sovedus.fs2.http.proxy.HttpProxyServerException

import cats.effect.Sync
import cats.syntax.all.*

import org.http4s.{Method, Request}

import fs2.{Chunk, Stream}
import scodec.bits.ByteVector

object RequestParser extends Parser {
  def parse[F[_]: Sync](read: F[Option[Chunk[Byte]]]): F[Request[F]] =
    for {
      (prelude, buffer2) <- recursiveRead(
        ByteVector.empty,
        read,
        ReqPreludeParser.ParserState.init
      )((state, buff) => ReqPreludeParser.parse(buff, state))(_.idx)
      (hp, buffer3) <- recursiveRead(buffer2, read, HeadersParser.ParserState.init)(
        (state, buff) => HeadersParser.parse(buff, state)
      )(_.idx).adaptError {
        case HttpProxyServerException.EmptyStream() =>
          HttpProxyServerException.ReachedEndOfStream()
      }
      body = requestBody(prelude.method, buffer3, read)
    } yield Request(
      method = prelude.method,
      uri = prelude.uri,
      httpVersion = prelude.httpVersion,
      headers = hp.headers,
      body = body
    )

  private def requestBody[F[_]](
      method: Method,
      head: ByteVector,
      read: F[Option[Chunk[Byte]]]
  ): Stream[F, Byte] =
    method match {
      case Method.CONNECT => Stream.empty
      case _ => bodyStream(head, read)
    }

}
