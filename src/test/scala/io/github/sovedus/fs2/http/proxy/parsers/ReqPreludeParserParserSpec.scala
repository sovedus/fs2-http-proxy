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

import cats.effect.IO
import cats.implicits.toTraverseOps

import org.http4s.{HttpVersion, Method, Uri}

import ReqPreludeParser.RequestPreludeParserException

class ReqPreludeParserParserSpec extends BaseSpec {

  "Request prelude parser" should "successfully parsed http request prelude line" in {
    val preludeData = for {
      method <- Method.all
      uri <- List("https://a.b.c.example.com", "/test")
      httpVersion <- List("HTTP/0.9", "HTTP/1.0", "HTTP/1.1", "HTTP/2.0")
    } yield (method, uri, httpVersion)

    preludeData
      .traverse {
        case (method, uri, httpVersion) =>
          val buffer =
            makeReqPrelude(method.name, uri, httpVersion).getBytes

          ReqPreludeParser.parse[IO](buffer, ReqPreludeParser.ParserState.init).asserting {
            result =>
              val reqPrelude = result.value
              reqPrelude.method shouldBe method
              reqPrelude.uri shouldBe Uri.fromString(uri).value
              reqPrelude.httpVersion shouldBe HttpVersion.fromString(httpVersion).value
          }
      }
      .map(_.head)

  }

  it should "return init state at call with empty buffer" in
    ReqPreludeParser.parse[IO](Array.empty[Byte], ReqPreludeParser.ParserState.init).asserting {
      result =>
        val state = result.left.value
        state shouldBe ReqPreludeParser.ParserState.init
    }

  it should "return partially parsed state" in {
    val partData = "GET http://exa".getBytes

    ReqPreludeParser.parse[IO](partData, ReqPreludeParser.ParserState.init).asserting {
      stateE =>
        val state = stateE.left.value
        state.idx shouldBe 14
        state.start shouldBe 4
        state.stage shouldBe 1
        state.method.value shouldBe Method.GET
        state.uri shouldBe None
        state.httpVersion shouldBe None
    }
  }

  it should "successfully parsed use partially parser state" in {
    val bytes = makeReqPrelude("GET", "http://example.com", "HTTP/1.0").getBytes

    val (part1, part2) = bytes.splitAt(20)

    ReqPreludeParser
      .parse[IO](part1, ReqPreludeParser.ParserState.init)
      .map(_.left.value)
      .flatMap(state => ReqPreludeParser.parse[IO](part1 ++ part2, state))
      .map(_.value)
      .asserting { result =>
        result.method shouldBe Method.GET
        result.uri shouldBe Uri.fromString("http://example.com").value
        result.httpVersion shouldBe HttpVersion.fromString("HTTP/1.0").value
      }
  }

  it should "failed when uses invalid HTTP method" in {
    val method = "/WRONG"
    val bytes = makeReqPrelude(method, "host:443", "HTTP/1.1").getBytes

    ReqPreludeParser
      .parse[IO](bytes, ReqPreludeParser.ParserState.init)
      .assertThrowsError[RequestPreludeParserException] { ex =>
        ex.method shouldBe None
        ex.uri shouldBe None
        ex.httpVersion shouldBe None
        ex.message should not be empty
        ex.throwable should not be null
      }
  }

  it should "failed when uses invalid uri" in {
    val bytes = makeReqPrelude("GET", "1234://host:port", "HTTP/1.1").getBytes

    ReqPreludeParser
      .parse[IO](bytes, ReqPreludeParser.ParserState.init)
      .assertThrowsError[RequestPreludeParserException] { ex =>
        ex.method.value shouldBe Method.GET
        ex.uri shouldBe None
        ex.httpVersion shouldBe None
        ex.message should not be empty
        ex.throwable should not be null
      }
  }

  it should "failed when uses invalid http version" in {
    val bytes = makeReqPrelude("GET", "host:123", "HTTP/500").getBytes

    ReqPreludeParser
      .parse[IO](bytes, ReqPreludeParser.ParserState.init)
      .assertThrowsError[RequestPreludeParserException] { ex =>
        ex.method.value shouldBe Method.GET
        ex.uri.value
        ex.httpVersion shouldBe None
        ex.message should not be empty
        ex.throwable should not be null
      }
  }

}
