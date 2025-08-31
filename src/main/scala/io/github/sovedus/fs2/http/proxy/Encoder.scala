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

import cats.effect.Sync

import org.http4s.{Request, Response}

object Encoder {
  private val SPACE = ' '
  private val CRLF = "\r\n"

  def encode[F[_]: Sync](req: Request[F]): fs2.Stream[F, Byte] = {
    val headRequestRaw = Sync[F].delay {
      val sb = new StringBuilder()
        .append(req.method.renderString)
        .append(SPACE)
        .append(req.uri.renderString)
        .append(SPACE)
        .append(req.httpVersion.renderString)
        .append(CRLF)

      req.headers.foreach(h =>
        sb.append(h.name).append(':').append(SPACE).append(h.sanitizedValue).append(CRLF): Unit
      )

      sb.append(CRLF).result()
    }

    fs2.Stream.eval(headRequestRaw).through(fs2.text.utf8.encode) ++ req.body
  }

  def encode[F[_]: Sync](resp: Response[F]): fs2.Stream[F, Byte] = {
    val headResponseRaw = Sync[F].delay {
      val sb = new StringBuilder()
        .append(resp.httpVersion.renderString)
        .append(SPACE)
        .append(resp.status.renderString)
        .append(CRLF)

      resp.headers.foreach(h =>
        sb.append(h.name).append(':').append(SPACE).append(h.sanitizedValue).append(CRLF): Unit
      )

      sb.append(CRLF).result()
    }

    fs2.Stream.eval(headResponseRaw).through(fs2.text.utf8.encode) ++ resp.body
  }
}
