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

import org.http4s.{HttpVersion, Method, Uri}

import scala.annotation.switch

private[parsers] object ReqPreludeParser extends Parser {

  final case class ParserResult(
      method: Method,
      uri: Uri,
      httpVersion: HttpVersion,
      idx: Int
  )

  final case class ParserState(
      idx: Int,
      start: Int,
      stage: Byte,
      complete: Boolean,
      error: Option[Throwable],
      method: Option[Method],
      uri: Option[Uri],
      httpVersion: Option[HttpVersion]
  )

  object ParserState {
    val init: ParserState = ParserState(
      idx = 0,
      start = 0,
      stage = 0,
      complete = false,
      error = None,
      method = None,
      uri = None,
      httpVersion = None)
  }

  final case class RequestPreludeParserException(
      message: String,
      throwable: Throwable,
      method: Option[Method],
      uri: Option[Uri],
      httpVersion: Option[HttpVersion]
  ) extends Exception(
        s"Parse http request prelude failed - Message: $message - Method: ${method.orNull} - Uri: ${uri.orNull} - httpVersion: ${httpVersion.orNull}",
        throwable
      )

  def parse[F[_]](buffer: Array[Byte], state: ParserState)(
      implicit F: Sync[F]
  ): F[Either[ParserState, ParserResult]] = F.defer {
    var idx = state.idx
    var start = state.start
    var stage = state.stage
    var complete = state.complete
    var throwable = state.error.orNull

    var method = state.method.orNull
    var uri = state.uri.orNull
    var httpVersion = state.httpVersion.orNull

    while (!complete && idx < buffer.length) {
      val value = buffer(idx)

      (stage: @switch) match {
        case 0 =>
          if (value == space) {
            Method.fromString(new String(buffer, start, idx - start)) match {
              case Left(ex) =>
                throwable = ex
                complete = true
              case Right(v) =>
                method = v
            }

            stage = 1
            start = idx + 1
          }
        case 1 =>
          if (value == space) {
            Uri.requestTarget(new String(buffer, start, idx - start)) match {
              case Left(ex) =>
                throwable = ex
                complete = true
              case Right(v) =>
                uri = v
            }

            stage = 2
            start = idx + 1
          }
        case 2 =>
          if (value == lf && (idx > 0 && buffer(idx - 1) == cr)) {
            HttpVersion.fromString(new String(buffer, start, idx - start - 1)) match {
              case Left(ex) =>
                throwable = ex
              case Right(v) =>
                httpVersion = v
            }
            complete = true
          }
      }

      idx += 1
    }

    if (throwable != null) {
      F.raiseError(
        RequestPreludeParserException(
          throwable.getMessage,
          throwable,
          Option(method),
          Option(uri),
          Option(httpVersion)))
    } else if (method == null || uri == null || httpVersion == null) {
      ParserState(
        idx,
        start,
        stage,
        complete,
        Option(throwable),
        Option(method),
        Option(uri),
        Option(httpVersion)
      ).asLeft.pure[F]
    } else {
      ParserResult(method, uri, httpVersion, idx).asRight.pure[F]
    }

  }

}
