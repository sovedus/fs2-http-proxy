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

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec

import org.http4s.Header
import org.scalatest.{EitherValues, OptionValues}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.ci.CIString

import scodec.bits.ByteVector

class HeadersParserParserSpec
    extends AsyncFlatSpec
    with AsyncIOSpec
    with Matchers
    with EitherValues
    with OptionValues {

  "Headers parser" should "parse one header" in {
    val data = ByteVector.encodeUtf8("Host: example.com\r\n\r\n").value
    val headerName = CIString("Host")
    val headerValue = "example.com"

    HeadersParser.parse[IO](data, HeadersParser.ParserState.init).asserting { result =>
      val headers = result.value.headers
      val header = headers.get(headerName).value
      header.head.name shouldBe headerName
      header.head.value shouldBe headerValue
    }
  }

  it should "return init state at call with empty buffer" in
    HeadersParser.parse[IO](ByteVector.empty, HeadersParser.ParserState.init).asserting {
      result =>
        val state = result.left.value
        state shouldBe HeadersParser.ParserState.init
    }

  it should "parse multiple headers" in {
    val data = ByteVector
      .encodeUtf8("Host: example.com\r\nHeader1: value1\r\nHeader2: value2\r\n\r\n")
      .value

    HeadersParser.parse[IO](data, HeadersParser.ParserState.init).asserting { result =>
      val headers = result.value.headers.headers
      val headerHost = headers.head
      val header1 = headers(1)
      val header2 = headers(2)

      headerHost shouldBe Header.Raw(CIString("Host"), "example.com")
      header1 shouldBe Header.Raw(CIString("Header1"), "value1")
      header2 shouldBe Header.Raw(CIString("Header2"), "value2")
    }
  }

  it should "return partially parsed state" in {
    val data = ByteVector.encodeUtf8("Host: example.com\r\nHeader1:").value

    HeadersParser.parse[IO](data, HeadersParser.ParserState.init).asserting { result =>
      val state = result.left.value

      state.idx should be > 0L
      state.start should be > 0L
      state.stage shouldBe true
      state.name.isDefined shouldBe true
      state.headers.size shouldBe 1
    }

  }

  it should "successfully parsed use partially parser state" in {
    val data1 = ByteVector.encodeUtf8("Host: example.com\r\nHe").value
    val data2 = ByteVector.encodeUtf8("ader1: value1\r\n\r\n").value

    val f = for {
      state <- HeadersParser.parse[IO](data1, HeadersParser.ParserState.init).map(_.left.value)
      result <- HeadersParser.parse[IO](data1 ++ data2, state)
    } yield result.value

    f.asserting(result => result.headers.headers.size shouldBe 2)
  }

}
