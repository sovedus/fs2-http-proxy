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

import org.http4s.{Header, Headers, HttpVersion, Method, Request, Response, Status}
import org.http4s.syntax.literals.*
import org.typelevel.ci.CIStringSyntax

class EncoderSpec extends BaseSpec {

  "Encoder" should "convert Request to Stream" in {
    val headers = List(Header.Raw(ci"Header1", "value1"), Header.Raw(ci"Header2", "value2"))
    val req = Request[IO](
      method = Method.POST,
      uri = uri"/create",
      httpVersion = HttpVersion.`HTTP/1.1`,
      headers = Headers(headers),
      body = fs2.Stream.emit("Request body").through(fs2.text.utf8.encode).covary[IO]
    )

    val expected = List(
      "POST /create HTTP/1.1\r\n",
      "Header1: value1\r\n",
      "Header2: value2\r\n",
      "\r\n",
      "Request body"
    ).mkString

    Encoder.encode[IO](req).through(fs2.text.utf8.decode).compile.string.asserting { result =>
      result shouldBe expected
    }
  }

  it should "convert Response to Stream" in {
    val headers = List(Header.Raw(ci"Header1", "value1"), Header.Raw(ci"Header2", "value2"))
    val resp = Response[IO](
      status = Status.Ok,
      httpVersion = HttpVersion.`HTTP/1.1`,
      headers = Headers(headers),
      body = fs2.Stream.emit("Response body").through(fs2.text.utf8.encode).covary[IO]
    )

    val expected = List(
      "HTTP/1.1 200 OK\r\n",
      "Header1: value1\r\n",
      "Header2: value2\r\n",
      "\r\n",
      "Response body"
    ).mkString

    Encoder.encode[IO](resp).through(fs2.text.utf8.decode).compile.string.asserting { result =>
      result shouldBe expected
    }

  }
}
