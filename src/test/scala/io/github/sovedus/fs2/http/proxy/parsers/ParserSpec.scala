package io.github.sovedus.fs2.http.proxy.parsers

import io.github.sovedus.fs2.http.proxy.BaseSpec
import io.github.sovedus.fs2.http.proxy.HttpProxyServerException.{
  EmptyStream,
  ReachedEndOfStream
}

import cats.effect.IO
import cats.implicits.catsSyntaxEitherId

import fs2.Chunk
import scodec.bits.ByteVector

class ParserSpec extends BaseSpec with Parser {

  "recursiveRead" should "thrown exception when buffer non empty but end data" in {
    val bytes = ByteVector(1, 2, 3)
    val read: IO[Option[Chunk[Byte]]] = IO(None)

    recursiveRead(bytes, read, "init")((_, _) => IO("part".asLeft[String]))(_ => 0)
      .assertThrows[ReachedEndOfStream]
  }

  it should "thrown exception when no data" in {
    val bytes = ByteVector.empty
    val read: IO[Option[Chunk[Byte]]] = IO(None)

    recursiveRead(bytes, read, "init")((_, _) => IO("part".asLeft[String]))(_ => 0)
      .assertThrows[EmptyStream]
  }
}
