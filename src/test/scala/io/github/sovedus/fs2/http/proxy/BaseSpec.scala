package io.github.sovedus.fs2.http.proxy

import cats.effect.{IO, Ref}
import cats.effect.testing.scalatest.AsyncIOSpec

import org.scalatest.{EitherValues, OptionValues}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.NoOpFactory

import fs2.Chunk

abstract class BaseSpec
    extends AsyncFlatSpec
    with AsyncIOSpec
    with Matchers
    with EitherValues
    with OptionValues {

  implicit protected val loggerFactory: LoggerFactory[IO] = NoOpFactory[IO]

  protected def toChunks(bytes: Array[Byte], chunkSize: Int): IO[IO[Option[Chunk[Byte]]]] =
    Ref[IO].of(0).map { ref =>
      ref.modify { pos =>
        if (pos < bytes.length) {
          (pos + chunkSize, Some(Chunk.array(bytes.slice(pos, pos + chunkSize))))
        } else (pos, None)
      }
    }

  protected def makeReqPrelude(method: String, uri: String, httpVersion: String) =
    s"$method $uri $httpVersion\r\n"
}
