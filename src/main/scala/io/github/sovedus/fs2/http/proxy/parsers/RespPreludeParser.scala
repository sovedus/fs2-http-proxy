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

import org.http4s.{HttpVersion, Status}

import scala.annotation.switch
import scala.util.{Failure, Success, Try}

import scodec.bits.ByteVector

object RespPreludeParser extends Parser {

  final case class ParserResult(
      httpVersion: HttpVersion,
      status: Status,
      idx: Long
  )

  final case class ParserState(
      idx: Long,
      start: Long,
      stage: Byte,
      complete: Boolean,
      error: Option[Throwable],
      httpVersion: Option[HttpVersion],
      code: Int
  )

  object ParserState {
    val init: ParserState = ParserState(
      idx = 0,
      start = 0,
      stage = 0,
      complete = false,
      error = None,
      httpVersion = None,
      code = 0
    )
  }

  final case class ResponsePreludeParserException(
      message: String,
      throwable: Throwable,
      httpVersion: Option[HttpVersion],
      code: Int
  ) extends Exception(
        s"Parse http response prelude failed - Message: $message - HttpVersion: ${httpVersion.orNull} - Code: $code",
        throwable
      )

  def parse[F[_]](buffer: ByteVector, state: ParserState)(
      implicit F: Sync[F]
  ): F[Either[ParserState, ParserResult]] = F.defer {
    var idx = state.idx
    var start = state.start
    var stage = state.stage
    var complete = state.complete
    var throwable = state.error.orNull

    var httpVersion = state.httpVersion.orNull
    var code = state.code

    var status: Status = null

    while (!complete && idx < buffer.length) {
      val value = buffer(idx)

      (stage: @switch) match {
        case 0 =>
          if (value == space) {
            HttpVersion.fromString(buffer.slice(start, idx).decodeUtf8Lenient) match {
              case Left(ex) =>
                throwable = ex
                complete = true
              case Right(v) =>
                httpVersion = v
            }

            stage = 1
            start = idx + 1
          }
        case 1 =>
          if (value == space) {
            Try(buffer.slice(start, idx).decodeUtf8Lenient.toInt) match {
              case Failure(ex) =>
                throwable = ex
                complete = true
              case Success(c) =>
                code = c
            }

            stage = 2
            start = idx + 1
          }
        case 2 =>
          if (value == lf && (idx > 0 && buffer(idx - 1) == cr)) {
            // Skip status phrase
            complete = true
          }
      }

      idx += 1
    }

    if (complete && throwable == null && code > 0) {
      Status.fromInt(code) match {
        case Left(ex) =>
          throwable = ex
        case Right(s) =>
          status = s
      }
    }

    if (complete && throwable != null) {
      F.raiseError(
        ResponsePreludeParserException(
          throwable.getMessage,
          throwable,
          Option(httpVersion),
          code
        )
      )
    } else if (!complete || httpVersion == null || code == 0) {
      ParserState(
        idx,
        start,
        stage,
        complete,
        Option(throwable),
        Option(httpVersion),
        code
      ).asLeft.pure[F]
    } else {
      ParserResult(httpVersion, status, idx).asRight.pure[F]
    }

  }
}
