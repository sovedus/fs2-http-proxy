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

import io.github.sovedus.fs2.http.proxy.HttpProxyServerException

import cats.effect.{Async, Sync}
import cats.syntax.all.*

import fs2.{Chunk, Pull, Stream}
import scodec.bits.ByteVector

trait Parser {
  private[parsers] val colon = 58 // ':'
  private[parsers] val space: Byte = 32 // space
  private[parsers] val lf: Byte = 10 // \n
  private[parsers] val cr: Byte = 13 // \r

  protected def recursiveRead[F[_]: Sync, S, A](
      buffer: ByteVector,
      read: F[Option[Chunk[Byte]]],
      state: S
  )(f: (S, ByteVector) => F[Either[S, A]])(skip: A => Long): F[(A, ByteVector)] =
    f(state, buffer).flatMap {
      case Left(state) =>
        read.flatMap {
          case Some(value) => recursiveRead(concatBytes(buffer, value), read, state)(f)(skip)
          case None if buffer.length > 0 =>
            Sync[F].raiseError(HttpProxyServerException.ReachedEndOfStream())
          case _ => Sync[F].raiseError(HttpProxyServerException.EmptyStream())
        }
      case Right(value) => (value, buffer.drop(skip(value))).pure[F]
    }

  protected def recursiveReadStream[F[_]: Async, S, A](
      buffer: ByteVector,
      stream: Stream[F, Byte],
      state: S
  )(
      f: (S, ByteVector) => F[Either[S, A]]
  )(skip: A => Long): F[(A, Stream[F, Byte])] =
    f(state, buffer).flatMap {
      case Left(nextState) =>
        stream.chunks.take(1).compile.last.flatMap {
          case Some(value) =>
            val newBuffer = concatBytes(buffer, value)
            val newStream = stream.drop(value.size.toLong)
            recursiveReadStream(newBuffer, newStream, nextState)(f)(skip)
          case None if buffer.length > 0 =>
            Async[F]
              .raiseError[(A, Stream[F, Byte])](HttpProxyServerException.ReachedEndOfStream())
          case _ =>
            Async[F].raiseError[(A, Stream[F, Byte])](HttpProxyServerException.EmptyStream())
        }
      case Right(value) =>
        Async[F].delay {
          val s = Stream.chunk(Chunk.byteVector(buffer.drop(skip(value)))) ++ stream
          (value, s)
        }
    }

  protected def bodyStream[F[_]](
      head: ByteVector,
      read: F[Option[Chunk[Byte]]]
  ): Stream[F, Byte] = {

    def go(head: ByteVector): Pull[F, Byte, Unit] = {
      val bytesPull = if (head.nonEmpty) {
        Pull.pure(Some(head))
      } else
        Pull.eval(read).map(_.map(_.toByteVector))

      bytesPull.flatMap {
        case Some(bytes) if head.nonEmpty =>
          Pull.output(Chunk.byteVector(bytes)) >> go(ByteVector.empty)
        case Some(bytes) => Pull.output(Chunk.byteVector(bytes)) >> go(head)
        case None => Pull.done
      }
    }

    go(head).stream
  }

  private def concatBytes(buffer: ByteVector, chunk: Chunk[Byte]): ByteVector =
    if (buffer.size == 0) {
      chunk.toByteVector
    } else {
      buffer ++ chunk.toByteVector
    }
}
