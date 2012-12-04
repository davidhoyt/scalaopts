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

import _root_.java.io.File
import Accumulators._
import common.StringUtil

/**
 * Holds option argument values as they're being processed.
 */
sealed trait OptionArgumentAccumulator[A, B, C] {
  def apply(value: Option[A], accumulator: B): B = accumulate(value, accumulator)
  def initialValue: B
  def done(accumulatedValues: B): C
  def accumulate(value: Option[A], accumulator: B): B
}

class CustomOptionArgumentAccumulator[A, B, C](val initialValue: B, val fnAccumulator: FnAccumulator[A, B], val onDone: FnAccumulatorDone[B, C]) extends OptionArgumentAccumulator[A, B, C] {
  def accumulate(value: Option[A], accumulator: B): B = fnAccumulator(value, accumulator)
  def done(accumulatedValues: B): C = onDone(accumulatedValues)
}

class ListOptionArgumentAccumulator[A](val initialValues: List[A] = List()) extends OptionArgumentAccumulator[A, List[Option[A]], List[Option[A]]] {
  def initialValue: List[Option[A]] = initialValues.map(Some(_)).reverse
  def accumulate(value: Option[A], accumulator: List[Option[A]]): List[Option[A]] = value :: accumulator
  def done(accumulatedValues: List[Option[A]]): List[Option[A]] = accumulatedValues.reverse
}

class SingleOptionArgumentAccumulator[A](val singleInitialValue: Option[A] = None) extends OptionArgumentAccumulator[A, Option[A], Option[A]] {
  def initialValue: Option[A] = singleInitialValue
  def accumulate(value: Option[A], accumulator: Option[A]): Option[A] = value
  def done(accumulatedValues: Option[A]): Option[A] = accumulatedValues
}

case class ByteList   (override val initialValues: List[Byte]    = List()) extends ListOptionArgumentAccumulator[Byte]   (initialValues)
case class ShortList  (override val initialValues: List[Short]   = List()) extends ListOptionArgumentAccumulator[Short]  (initialValues)
case class IntegerList(override val initialValues: List[Int]     = List()) extends ListOptionArgumentAccumulator[Int]    (initialValues)
case class LongList   (override val initialValues: List[Long]    = List()) extends ListOptionArgumentAccumulator[Long]   (initialValues)
case class FloatList  (override val initialValues: List[Float]   = List()) extends ListOptionArgumentAccumulator[Float]  (initialValues)
case class DoubleList (override val initialValues: List[Double]  = List()) extends ListOptionArgumentAccumulator[Double] (initialValues)
case class BooleanList(override val initialValues: List[Boolean] = List()) extends ListOptionArgumentAccumulator[Boolean](initialValues)
case class CharList   (override val initialValues: List[Char]    = List()) extends ListOptionArgumentAccumulator[Char]   (initialValues)
case class StringList (override val initialValues: List[String]  = List()) extends ListOptionArgumentAccumulator[String] (initialValues)
case class FileList   (override val initialValues: List[File]    = List()) extends ListOptionArgumentAccumulator[File]   (initialValues)

case class SingleByte   (val initialAccumulatorValue: Byte         = 0               ) extends SingleOptionArgumentAccumulator[Byte]   (Some(initialAccumulatorValue))
case class SingleShort  (val initialAccumulatorValue: Short        = 0               ) extends SingleOptionArgumentAccumulator[Short]  (Some(initialAccumulatorValue))
case class SingleInteger(val initialAccumulatorValue: Int          = 0               ) extends SingleOptionArgumentAccumulator[Int]    (Some(initialAccumulatorValue))
case class SingleLong   (val initialAccumulatorValue: Long         = 0L              ) extends SingleOptionArgumentAccumulator[Long]   (Some(initialAccumulatorValue))
case class SingleFloat  (val initialAccumulatorValue: Float        = 0.0f            ) extends SingleOptionArgumentAccumulator[Float]  (Some(initialAccumulatorValue))
case class SingleDouble (val initialAccumulatorValue: Double       = 0.0D            ) extends SingleOptionArgumentAccumulator[Double] (Some(initialAccumulatorValue))
case class SingleBoolean(val initialAccumulatorValue: Boolean      = false           ) extends SingleOptionArgumentAccumulator[Boolean](Some(initialAccumulatorValue))
case class SingleChar   (val initialAccumulatorValue: Char         = '\0'            ) extends SingleOptionArgumentAccumulator[Char]   (Some(initialAccumulatorValue))
case class SingleString (val initialAccumulatorValue: String       = StringUtil.empty) extends SingleOptionArgumentAccumulator[String] (Some(initialAccumulatorValue))
case class SingleFile   (val initialAccumulatorValue: Option[File] = None            ) extends SingleOptionArgumentAccumulator[File]   (initialAccumulatorValue)
