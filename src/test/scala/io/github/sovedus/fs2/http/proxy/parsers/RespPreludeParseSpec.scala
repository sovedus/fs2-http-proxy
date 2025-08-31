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

import org.http4s.{HttpVersion, ParseFailure, Status}

import scodec.bits.ByteVector

import RespPreludeParser.ResponsePreludeParserException

class RespPreludeParseSpec extends BaseSpec {

  "RespPreludeParser" should "successfully parse resp prelude with reason status" in {
    val bytes = ByteVector.encodeUtf8("HTTP/1.1 200 Ok\r\n").value

    RespPreludeParser.parse[IO](bytes, RespPreludeParser.ParserState.init).asserting { result =>
      val prelude = result.value

      prelude.status shouldBe Status.Ok
      prelude.httpVersion shouldBe HttpVersion.`HTTP/1.1`
    }
  }

  it should "failed when uses invalid http version" in {
    val bytes = ByteVector.encodeUtf8("HTTP/5 200 Ok\r\n").value

    RespPreludeParser
      .parse[IO](bytes, RespPreludeParser.ParserState.init)
      .assertThrowsError[ResponsePreludeParserException] { ex =>
        ex.getCause shouldBe a[ParseFailure]

        val cause = ex.getCause.asInstanceOf[ParseFailure]
        cause.message should startWith("HTTP version: HTTP/5")

      }
  }

  it should "failed when uses not number http status" in {
    val bytes = ByteVector.encodeUtf8("HTTP/1.1 OOO Ok\r\n").value

    RespPreludeParser
      .parse[IO](bytes, RespPreludeParser.ParserState.init)
      .assertThrowsError[ResponsePreludeParserException] { ex =>
        ex.httpVersion.value shouldBe HttpVersion.`HTTP/1.1`
        ex.getCause shouldBe a[NumberFormatException]
      }
  }

  it should "failed when uses status number out of accept range" in {
    val bytes = ByteVector.encodeUtf8("HTTP/1.1 9999 Ok\r\n").value

    RespPreludeParser
      .parse[IO](bytes, RespPreludeParser.ParserState.init)
      .asserting(_ => succeed)
      .assertThrowsError[ResponsePreludeParserException] { ex =>
        ex.httpVersion.value shouldBe HttpVersion.`HTTP/1.1`
        ex.getCause shouldBe a[ParseFailure]
        ex.getCause.getMessage shouldBe "Invalid code: 9999 is not between 100 and 599."
      }
  }

  it should "successfully parsed use partially parser state" in {
    val bytes = ByteVector.encodeUtf8("HTTP/1.1 200 Ok\r\n").value

    val (part1, part2) = bytes.splitAt(4)

    RespPreludeParser
      .parse[IO](part1, RespPreludeParser.ParserState.init)
      .map(_.left.value)
      .flatMap(state => RespPreludeParser.parse[IO](part1 ++ part2, state))
      .map(_.value)
      .asserting { result =>
        result.status shouldBe Status.Ok
        result.httpVersion shouldBe HttpVersion.`HTTP/1.1`
        result.idx should be > 0L
      }
  }
}
