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

import common._

/**
 * Defines common accumulator function types for option arguments.
 */
object AccumulatorFunctionTypes {
  /**
   * Function that accepts a value and processes it or references it or something.
   */
  type FnAccumulator[A, B] = (A, B) => B
  type FnAccumulatorDone[B, +C] = B => C
  type FnAsyncAccumulatorCallback[A] = A => Unit
  type FnAsyncAccumulatorDone = Unit => Unit
}

import AccumulatorFunctionTypes._

/**
 * Holds option argument values as they're being processed.
 */
sealed trait OptionArgumentAccumulator[+A, +B, +C] {
  def apply[X >: A, Y >: B, Z >: C](value: X, accumulator: Y): Z = accumulate(value, accumulator)
  def initialValue: B
  def done[Y >: B](accumulatedValues: Y): C
  def accumulate[X >: A, Y >: B, Z >: C](value: X, accumulator: Y): Z
}

class CustomOptionArgumentAccumulator[A, B, C](val initialValue: B, val fnAccumulator: FnAccumulator[A, B], val onDone: FnAccumulatorDone[B, C]) extends OptionArgumentAccumulator[A, B, C] {
  def accumulate[X >: A, Y >: B, Z >: C](value: X, accumulator: Y): Z = fnAccumulator(value.asInstanceOf[A], accumulator.asInstanceOf[B]).asInstanceOf[Z]
  def done[Y >: B](accumulatedValues: Y): C = onDone(accumulatedValues.asInstanceOf[B])
}

class AsyncOptionArgumentAccumulator[A](val initialValues: List[A] = List(), val callback: FnAsyncAccumulatorCallback[A], val doneCallback: Option[FnAsyncAccumulatorDone] = None) extends OptionArgumentAccumulator[A, Unit, Unit] {
  def initialValue: Unit = {
    for (value <- initialValues)
      callback(value)
  }
  def accumulate[X >: A, Y >: Unit, Z >: Unit](value: X, accumulator: Y) = callback(value.asInstanceOf[A])
  def done[Unit](accumulatedValues: Unit) =
    if (doneCallback.isDefined) {
      doneCallback.get()
    }
}

class ListOptionArgumentAccumulator[+A](val initialValues: List[A] = List()) extends OptionArgumentAccumulator[A, List[A], List[A]] {
  def initialValue: List[A] = initialValues.reverse
  def accumulate[X >: A, Y >: List[A], Z >: List[A]](value: X, accumulator: Y) = (value :: accumulator.asInstanceOf[List[A]]).asInstanceOf[Z]
  def done[Y >: List[A]](accumulatedValues: Y): List[A] = accumulatedValues.asInstanceOf[List[A]].reverse
}

class SingleOptionArgumentAccumulator[+A](val singleInitialValue: A) extends OptionArgumentAccumulator[A, A, A] {
  def initialValue: A = singleInitialValue
  def accumulate[X >: A, Y >: A, Z >: A](value: X, accumulator:Y): Z = value.asInstanceOf[A]
  def done[Y >: A](accumulatedValues: Y): A = accumulatedValues.asInstanceOf[A]
}

/**
 * Necessary because context bounds desugar to an implicit parameter and implicit parameter lists
 * always come last meaning that the implicit isn't created until after we're trying to use it.
 *
 * e.g.:
 *   Given something like:
 *     class foo[A: Default](val bar: A = default[A])
 *
 *   Is desugared into:
 *     class foo[A](val bar: A = d)(implicit d: Default[A])
 *
 *   The workaround is to do something like:
 *     class foo[A](val bar: A)
 *     object foo {
 *       def apply[A : Default]() = new foo(default)
 *     }
 */
object SingleOptionArgumentAccumulator {
  def apply[A: Default]() = new SingleOptionArgumentAccumulator[A](default)
}
