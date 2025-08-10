package io.github.sovedus.fs2.http.proxy.parsers

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec

import org.http4s.Header
import org.scalatest.{EitherValues, OptionValues}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.ci.CIString

class HeadersParserParserSpec
    extends AsyncFlatSpec
    with AsyncIOSpec
    with Matchers
    with EitherValues
    with OptionValues {

  "Headers parser" should "parse one header" in {
    val data = "Host: example.com\r\n\r\n".getBytes
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
    HeadersParser.parse[IO](Array.empty[Byte], HeadersParser.ParserState.init).asserting {
      result =>
        val state = result.left.value
        state shouldBe HeadersParser.ParserState.init
    }

  it should "parse multiple headers" in {
    val data = "Host: example.com\r\nHeader1: value1\r\nHeader2: value2\r\n\r\n".getBytes

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
    val data = "Host: example.com\r\nHeader1:".getBytes

    HeadersParser.parse[IO](data, HeadersParser.ParserState.init).asserting { result =>
      val state = result.left.value

      state.idx should be > 0
      state.start should be > 0
      state.stage shouldBe true
      state.name.isDefined shouldBe true
      state.headers.size shouldBe 1
    }

  }

  it should "successfully parsed use partially parser state" in {
    val data1 = "Host: example.com\r\nHe".getBytes
    val data2 = "ader1: value1\r\n\r\n".getBytes

    val f = for {
      state <- HeadersParser.parse[IO](data1, HeadersParser.ParserState.init).map(_.left.value)
      result <- HeadersParser.parse[IO](data1 ++ data2, state)
    } yield result.value

    f.asserting(result => result.headers.headers.size shouldBe 2)
  }

}
