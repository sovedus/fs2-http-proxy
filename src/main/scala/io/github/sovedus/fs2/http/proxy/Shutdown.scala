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

package io.github.sovedus.fs2.http.proxy

import cats.effect.Ref
import cats.effect.implicits.{genTemporalOps_, monadCancelOps_}
import cats.effect.kernel.{Deferred, Temporal}
import cats.syntax.all.*

import scala.concurrent.duration.{Duration, FiniteDuration}

trait Shutdown[F[_]] {

  def shutdown: F[Unit]

  def signal: F[Unit]

  def trackConnection: fs2.Stream[F, Unit]

}

object Shutdown {
  private case class State(isShutdown: Boolean, activeConnections: Int) {
    def startConnection(): State = copy(activeConnections = activeConnections + 1)

    def endConnection(): State = copy(activeConnections = activeConnections - 1)
  }

  def apply[F[_]](timeout: Duration)(implicit F: Temporal[F]): F[Shutdown[F]] =
    timeout match {
      case fd: FiniteDuration =>
        if (fd.length > 0) timedShutdown(timeout) else immediateShutdown
      case _ => timedShutdown(timeout)
    }

  private def timedShutdown[F[_]](timeout: Duration)(implicit F: Temporal[F]): F[Shutdown[F]] =
    for {
      shutdownSignal <- Deferred[F, Unit]
      done <- Deferred[F, Unit]
      state <- Ref[F].of(State(isShutdown = false, 0))
    } yield new Shutdown[F] {

      override def shutdown: F[Unit] = shutdownSignal
        .complete(())
        .flatMap { _ =>
          state.modify { s =>
            val f = if (s.activeConnections == 0) {
              F.unit
            } else {
              timeout match {
                case fd: FiniteDuration => done.get.timeoutTo(fd, F.unit)
                case _ => done.get
              }
            }

            s.copy(isShutdown = true) -> f
          }
        }
        .flatten
        .uncancelable

      override def signal: F[Unit] = shutdownSignal.get

      override def trackConnection: fs2.Stream[F, Unit] =
        fs2.Stream.bracket(incrementConnection)(_ => decrementConnection)

      private def incrementConnection: F[Unit] =
        state.update(_.startConnection())

      private def decrementConnection: F[Unit] = state
        .modify { state =>
          val newState = state.endConnection()

          val f = if (state.isShutdown && newState.activeConnections <= 0) {
            done.complete(()).void
          } else {
            F.unit
          }

          newState -> f
        }
        .flatten
        .uncancelable
    }

  private def immediateShutdown[F[_]](implicit F: Temporal[F]): F[Shutdown[F]] =
    Deferred[F, Unit].map { done =>
      new Shutdown[F] {
        override def shutdown: F[Unit] = done.complete(()).void

        override def signal: F[Unit] = done.get

        override def trackConnection: fs2.Stream[F, Unit] = fs2.Stream.unit
      }
    }
}
