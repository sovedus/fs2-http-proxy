package io.github.sovedus.fs2.http.proxy.parsers

import io.github.sovedus.fs2.http.proxy.BaseSpec

import cats.effect.IO

import org.http4s.{HttpVersion, ParseFailure, Status}

import RespPreludeParser.ResponsePreludeParserException

class RespPreludeParseSpec extends BaseSpec {

  "RespPreludeParser" should "successfully parse resp prelude with reason status" in {
    val bytes = "HTTP/1.1 200 Ok\r\n".getBytes

    RespPreludeParser.parse[IO](bytes, RespPreludeParser.ParserState.init).asserting { result =>
      val prelude = result.value

      prelude.status shouldBe Status.Ok
      prelude.httpVersion shouldBe HttpVersion.`HTTP/1.1`
    }
  }

  it should "failed when uses invalid http version" in {
    val bytes = "HTTP/5 200 Ok\r\n".getBytes

    RespPreludeParser
      .parse[IO](bytes, RespPreludeParser.ParserState.init)
      .assertThrowsError[ResponsePreludeParserException] { ex =>
        ex.getCause shouldBe a[ParseFailure]

        val cause = ex.getCause.asInstanceOf[ParseFailure]
        cause.message should startWith("HTTP version: HTTP/5")

      }
  }

  it should "failed when uses not number http status" in {
    val bytes = "HTTP/1.1 OOO Ok\r\n".getBytes

    RespPreludeParser
      .parse[IO](bytes, RespPreludeParser.ParserState.init)
      .assertThrowsError[ResponsePreludeParserException] { ex =>
        ex.httpVersion.value shouldBe HttpVersion.`HTTP/1.1`
        ex.getCause shouldBe a[NumberFormatException]
      }
  }

  it should "failed when uses status number out of accept range" in {
    val bytes = "HTTP/1.1 9999 Ok\r\n".getBytes

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
    val bytes = "HTTP/1.1 200 Ok\r\n".getBytes

    val (part1, part2) = bytes.splitAt(4)

    RespPreludeParser
      .parse[IO](part1, RespPreludeParser.ParserState.init)
      .map(_.left.value)
      .flatMap(state => RespPreludeParser.parse[IO](part1 ++ part2, state))
      .map(_.value)
      .asserting { result =>
        result.status shouldBe Status.Ok
        result.httpVersion shouldBe HttpVersion.`HTTP/1.1`
        result.idx should be > 0
      }
  }
}
