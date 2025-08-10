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
