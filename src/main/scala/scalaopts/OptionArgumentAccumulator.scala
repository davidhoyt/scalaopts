/*
  Copyright (C) 2012-2013 the original author or authors.

  See the LICENSE.txt file distributed with this work for additional
  information regarding copyright ownership.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*/

package scalaopts

import Accumulators._

/**
 * Holds option argument values as they're being processed.
 */
sealed trait OptionArgumentAccumulator[A, B, C] {
  def apply(value: Option[A]): B = accumulate(value)
  def done(accumulatedValues: B): C
  def accumulate(value: Option[A]): B
}

class CustomOptionArgumentAccumulator[A, B, C](val accumulator: FnAccumulator[A, B], val onDone: FnAccumulatorDone[B, C]) extends OptionArgumentAccumulator[A, B, C] {
  def accumulate(value: Option[A]): B = accumulator(value)
  def done(accumulatedValues: B): C = onDone(accumulatedValues)
}

case class ListOptionArgumentAccumulator[A]() extends OptionArgumentAccumulator[A, List[Option[A]], List[Option[A]]] {
  private[ListOptionArgumentAccumulator] val arguments = List[Option[A]]()

  def accumulate(value: Option[A]) = value :: arguments
  def done(accumulatedValues: List[Option[A]]): List[Option[A]] = arguments.reverse
}