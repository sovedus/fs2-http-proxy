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

import cats.effect.Sync
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxEitherId}

import org.http4s.{Header, Headers}
import org.typelevel.ci.CIString

private[parsers] object HeadersParser extends Parser {

  final case class ParserResult(headers: Headers, idx: Int)

  final case class ParserState(
      idx: Int,
      start: Int,
      stage: Boolean,
      complete: Boolean,
      name: Option[String],
      headers: List[Header.Raw]
  )

  object ParserState {
    val init: ParserState = ParserState(
      idx = 0,
      start = 0,
      stage = false,
      complete = false,
      name = None,
      headers = Nil
    )
  }

  def parse[F[_]](buffer: Array[Byte], state: ParserState)(
      implicit F: Sync[F]
  ): F[Either[ParserState, ParserResult]] = F.defer {
    var idx = state.idx
    var start = state.start
    var stage = state.stage
    var complete = state.complete
    var name = state.name.orNull
    var headers = state.headers

    while (!complete && idx < buffer.length) {
      val current = buffer(idx)

      if (!stage) {
        if (current == colon) {
          name = new String(buffer, start, idx - start)
          start = idx + 1
          stage = true
        } else if (current == lf && buffer(idx - 1) == cr) {
          complete = true
        }
      } else {
        if (current == lf && buffer(idx - 1) == cr) {
          val ciName = CIString(name)
          name = null
          val value = new String(buffer, start, idx - start - 1).trim
          val header = Header.Raw(ciName, value)
          headers = headers :+ header
          start = idx + 1
          stage = false
        }
      }

      idx += 1
    }

    if (!complete) {
      ParserState(
        idx = idx,
        start = start,
        stage = stage,
        complete = complete,
        name = Option(name),
        headers = headers
      ).asLeft.pure[F]
    } else {
      ParserResult(Headers(headers), idx).asRight.pure[F]
    }

  }
}
