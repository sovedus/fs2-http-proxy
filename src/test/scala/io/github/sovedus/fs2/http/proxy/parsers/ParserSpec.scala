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
