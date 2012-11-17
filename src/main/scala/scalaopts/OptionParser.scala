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

import common.StringUtil
import StringTransforms._

/**
 * Holds information that will be used later when arguments are evaluated. Used in a builder fashion and meant to
 * be used in the internal DSL.
 *
 * @tparam A Result type of applying the transform.
 */
sealed trait OptionParser[+A] {
  def withDefault(default: Any): OptionParser[A]
  def withTransform(transform: FnTransform[Any]): OptionParser[A]
  def apply(value: String): Option[Any]
}

/**
 * Defines a custom parser that transforms a [[java.lang.String]] into a value.
 *
 * @param default The default value to use if the argument is not defined.
 * @param requiresAssociatedValue True if, when processed, the next value parsed should be applied to this instance.
 * @param transform Function that transforms the [[java.lang.String]] into a value.
 * @tparam A Result type of applying the transform.
 */
class CustomOptionParser[+A](default: Option[Any] = None, requiresAssociatedValue: Boolean = true, transform: FnTransform[Any]) extends OptionParser[A] {
  def withDefault(default_value: Any)                = new CustomOptionParser(Some(default_value), requiresAssociatedValue, transform)
  def withTransform(new_transform: FnTransform[Any]) = new CustomOptionParser(default, requiresAssociatedValue, new_transform)
  def withRequiresAssociatedValue(value: Boolean)    = new CustomOptionParser(default, value, transform)
  def apply(value: String) = transform(value)
}

case class ByteOption(defaultValue: Byte = 0) extends CustomOptionParser[Byte](Some(defaultValue), true, TRANSFORM_BYTE)
object DefaultByteOption extends ByteOption()

case class ShortOption(defaultValue: Short = 0) extends CustomOptionParser[Short](Some(defaultValue), true, TRANSFORM_SHORT)
object DefaultShortOption extends ShortOption()

case class IntegerOption(defaultValue: Int = 0) extends CustomOptionParser[Int](Some(defaultValue), true, TRANSFORM_INT)
object DefaultIntegerOption extends IntegerOption()

case class LongOption(defaultValue: Long = 0L) extends CustomOptionParser[Long](Some(defaultValue), true, TRANSFORM_LONG)
object DefaultLongOption extends LongOption()

case class FloatOption(defaultValue: Float = 0.0f) extends CustomOptionParser[Float](Some(defaultValue), true, TRANSFORM_FLOAT)
object DefaultFloatOption extends FloatOption()

case class DoubleOption(defaultValue: Double = 0.0D) extends CustomOptionParser[Double](Some(defaultValue), true, TRANSFORM_DOUBLE)
object DefaultDoubleOption extends DoubleOption()

case class BooleanOption(defaultValue: Boolean = false) extends CustomOptionParser[Boolean](Some(defaultValue), true, TRANSFORM_BOOLEAN)
object DefaultBooleanOption extends BooleanOption()

case class FlagOption() extends CustomOptionParser[Boolean](Some(true), false, TRANSFORM_NOOP)
object DefaultFlagOption extends FlagOption()

case class CharOption(defaultValue: Char = '\0') extends CustomOptionParser[Char](Some(defaultValue), true, TRANSFORM_CHAR)
object DefaultCharOption extends CharOption()

case class StringOption(defaultValue: String = StringUtil.empty) extends CustomOptionParser[String](Some(defaultValue), true, TRANSFORM_STRING)
object DefaultStringOption extends StringOption()
