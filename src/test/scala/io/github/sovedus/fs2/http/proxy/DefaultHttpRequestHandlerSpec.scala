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

import cats.effect.IO

import org.http4s.{Header, Method, Request}
import org.http4s.syntax.literals.*
import org.typelevel.ci.CIStringSyntax
import org.typelevel.log4cats.slf4j.Slf4jLogger

class DefaultHttpRequestHandlerSpec extends BaseSpec {

  private val logger = Slf4jLogger.getLogger[IO]

  "DefaultHttpRequestHandle" should "successfully forward request and return response" in {
    val handler = HttpRequestHandler.default[IO](logger)

    val req = Request[IO]()
      .withMethod(Method.GET)
      .withUri(uri"http://google.com")
      .withHeaders(Header.Raw(ci"Host", "google.com"))
      .withHeaders(Header.Raw(ci"User-Agent", "curl/8.15.0"))
      .withHeaders(Header.Raw(ci"Accept", "*/*"))

    handler.handleRequest(req).asserting(resp => resp.status.isSuccess shouldBe true)
  }

}
