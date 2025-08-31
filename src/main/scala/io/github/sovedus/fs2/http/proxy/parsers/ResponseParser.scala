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

import cats.effect.{Async, Sync}
import cats.syntax.all.*

import org.http4s.Response

import fs2.*
import scodec.bits.ByteVector

object ResponseParser extends Parser {

  def parse[F[_]: Async](stream: Stream[F, Byte]): F[Response[F]] = {
    for {
      (prelude, stream2) <- recursiveReadStream(
        ByteVector.empty,
        stream,
        RespPreludeParser.ParserState.init
      )((state, buff) => RespPreludeParser.parse(buff, state))(_.idx)
      (hp, stream3) <- recursiveReadStream(
        ByteVector.empty,
        stream2,
        HeadersParser.ParserState.init
      )((state, buff) => HeadersParser.parse(buff, state))(_.idx).adaptError {
        case HttpProxyServerException.EmptyStream() =>
          HttpProxyServerException.ReachedEndOfStream()
      }
    } yield Response(prelude.status, prelude.httpVersion, hp.headers, stream3)
  }

  def parse[F[_]: Sync](read: F[Option[Chunk[Byte]]]): F[Response[F]] =
    for {
      (prelude, buffer2) <- recursiveRead(
        ByteVector.empty,
        read,
        RespPreludeParser.ParserState.init
      )((state, buff) => RespPreludeParser.parse(buff, state))(_.idx)
      (hp, buffer3) <- recursiveRead(buffer2, read, HeadersParser.ParserState.init)(
        (state, buff) => HeadersParser.parse(buff, state)
      )(_.idx).adaptError {
        case HttpProxyServerException.EmptyStream() =>
          HttpProxyServerException.ReachedEndOfStream()
      }
      body = bodyStream(buffer3, read)
    } yield Response(prelude.status, prelude.httpVersion, hp.headers, body)

}
