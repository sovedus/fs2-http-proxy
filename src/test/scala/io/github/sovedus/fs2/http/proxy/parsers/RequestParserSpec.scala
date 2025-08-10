package io.github.sovedus.fs2.http.proxy.parsers

import io.github.sovedus.fs2.http.proxy.BaseSpec

import cats.effect.IO

import org.http4s.{HttpVersion, Method}

class RequestParserSpec extends BaseSpec {

  "RequestParser" should "parse request and return Request with non empty body" in {
    val reqBytes = List(
      "GET host:443 HTTP/1.1\r\n",
      "Host: host:443\r\n",
      "Header1: value1\r\n",
      "\r\n",
      "Request body"
    ).mkString.getBytes

    toChunks(reqBytes, 5).flatMap(RequestParser.parse[IO]).flatMap { result =>
      result.method shouldBe Method.GET
      result.uri.toString() shouldBe "host:443"
      result.httpVersion shouldBe HttpVersion.`HTTP/1.1`
      result.headers.headers.size shouldBe 2

      result.body.through(fs2.text.utf8.decode).compile.string.map { body =>
        body shouldBe "Request body"
      }
    }
  }

  it should "parse request and return Request with empty body" in {
    val reqBytes = List(
      "CONNECT host:443 HTTP/1.1\r\n",
      "Host: host:443\r\n",
      "Header1: value1\r\n",
      "\r\n",
      "Request body"
    ).mkString.getBytes

    toChunks(reqBytes, 5).flatMap(RequestParser.parse[IO]).flatMap { result =>
      result.method shouldBe Method.CONNECT
      result.uri.toString() shouldBe "host:443"
      result.httpVersion shouldBe HttpVersion.`HTTP/1.1`
      result.headers.headers.size shouldBe 2

      result.body.through(fs2.text.utf8.decode).compile.string.map(body => body shouldBe empty)
    }
  }

}
