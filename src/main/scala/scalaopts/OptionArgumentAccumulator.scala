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

/**
 * Defines common accumulator function types for option arguments.
 */
object AccumulatorFunctionTypes {
  /**
   * Function that accepts a value and processes it or references it or something.
   */
  type FnAccumulator[A, B] = (Option[A], B) => B
  type FnAccumulatorDone[B, +C] = B => C
  type FnAsyncAccumulatorCallback[A] = Option[A] => Unit
  type FnAsyncAccumulatorDone = Unit => Unit
}

import AccumulatorFunctionTypes._

/**
 * Holds option argument values as they're being processed.
 */
sealed trait OptionArgumentAccumulator[+A, +B, +C] {
  def apply[X >: A, Y >: B, Z >: C](value: Option[X], accumulator: Y): Z = accumulate(value, accumulator)
  def initialValue: B
  def done[Y >: B](accumulatedValues: Y): C
  def accumulate[X >: A, Y >: B, Z >: C](value: Option[X], accumulator: Y): Z
}

class CustomOptionArgumentAccumulator[A, B, C](val initialValue: B, val fnAccumulator: FnAccumulator[A, B], val onDone: FnAccumulatorDone[B, C]) extends OptionArgumentAccumulator[A, B, C] {
  def accumulate[X >: A, Y >: B, Z >: C](value: Option[X], accumulator: Y): Z = fnAccumulator(value.asInstanceOf[Option[A]], accumulator.asInstanceOf[B]).asInstanceOf[Z]
  def done[Y >: B](accumulatedValues: Y): C = onDone(accumulatedValues.asInstanceOf[B])
}

class AsyncOptionArgumentAccumulator[A](val initialValues: List[A] = List(), val callback: FnAsyncAccumulatorCallback[A], val doneCallback: Option[FnAsyncAccumulatorDone] = None) extends OptionArgumentAccumulator[A, Unit, Unit] {
  def initialValue: Unit = {
    for (value <- initialValues)
      callback(Some(value))
  }
  def accumulate[X >: A, Y >: Unit, Z >: Unit](value: Option[X], accumulator: Y) = callback(value.asInstanceOf[Option[A]])
  def done[Unit](accumulatedValues: Unit) =
    if (doneCallback.isDefined) {
      doneCallback.get()
    }
}

class ListOptionArgumentAccumulator[+A](val initialValues: List[A] = List()) extends OptionArgumentAccumulator[A, List[Option[A]], List[Option[A]]] {
  def initialValue: List[Option[A]] = initialValues.map(Some(_)).reverse
  def accumulate[X >: A, Y >: List[Option[A]], Z >: List[Option[A]]](value: Option[X], accumulator: Y) = (value :: accumulator.asInstanceOf[List[Option[A]]]).asInstanceOf[Z]
  def done[Y >: List[Option[A]]](accumulatedValues: Y): List[Option[A]] = accumulatedValues.asInstanceOf[List[Option[A]]].reverse
}

class SingleOptionArgumentAccumulator[+A](val singleInitialValue: Option[A] = None) extends OptionArgumentAccumulator[A, Option[A], Option[A]] {
  def initialValue: Option[A] = singleInitialValue
  def accumulate[X >: A, Y >: Option[A], Z >: Option[A]](value: Option[X], accumulator:Y): Z = value.asInstanceOf[Option[A]]
  def done[Y >: Option[A]](accumulatedValues: Y): Option[A] = accumulatedValues.asInstanceOf[Option[A]]
}
