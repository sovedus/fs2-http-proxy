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

import scala.concurrent.duration.{Duration, DurationInt}

object Defaults {

  val MAX_CONNECTIONS: Int = 1024
  val RECEIVE_BUFFER_SIZE: Int = 16 * 1024
  val SHUTDOWN_TIMEOUT: Duration = 30.seconds

  def errorHandler[F[_]]: PartialFunction[Throwable, F[Unit]] = PartialFunction.empty
}
