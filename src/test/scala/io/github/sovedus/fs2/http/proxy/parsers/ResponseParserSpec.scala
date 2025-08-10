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

import io.github.sovedus.fs2.http.proxy.BaseSpec

import org.http4s.{HttpVersion, Status}

class ResponseParserSpec extends BaseSpec {

  "ResponseParser" should "successfully parse http response" in {
    val bytes = List(
      "HTTP/1.1 200 OK\r\n",
      "Header1: value1\r\n",
      "Header2: value2\r\n",
      "\r\n",
      "Response body"
    ).mkString.getBytes

    toChunks(bytes, 5).flatMap { read =>
      ResponseParser.parse(read).flatMap { resp =>
        resp.httpVersion shouldBe HttpVersion.`HTTP/1.1`
        resp.status shouldBe Status.Ok
        resp.headers.headers.size shouldBe 2

        resp.body.through(fs2.text.utf8.decode).compile.string.map(_ shouldBe "Response body")
      }
    }
  }
}
