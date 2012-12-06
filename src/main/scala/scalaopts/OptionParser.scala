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

import OptionParserStringTransforms._

/**
 * Holds information that will be used later when arguments are evaluated. Used in a builder fashion and meant to
 * be used in the internal DSL.
 *
 * @tparam A Result type of applying the transform.
 */
sealed trait OptionParser[+A] {
  def apply(value: String): Option[A]
}

/**
 * Defines a custom parser that transforms a [[java.lang.String]] into a value.
 *
 * @param optionDefaultValue The default value to use if the argument is not defined.
 * @param requiresAssociatedValue True if, when processed, the next value parsed should be applied to this instance.
 * @param transform Function that transforms the [[java.lang.String]] into a value.
 * @tparam A Result type of applying the transform.
 */
class CustomOptionParser[+A](val optionDefaultValue: Option[A] = None, val useDefaultValue: Boolean = true, val requiresAssociatedValue: Boolean = true, val transform: FnTransform[A]) extends OptionParser[A] {
  def withDefault[X >: A](default_value: X): CustomOptionParser[X]                = new CustomOptionParser(Some(default_value), useDefaultValue, requiresAssociatedValue, transform)
  def withTransform[X >: A](new_transform: FnTransform[X]): CustomOptionParser[X] = new CustomOptionParser(optionDefaultValue,  useDefaultValue, requiresAssociatedValue, new_transform)
  def withUseDefaultValue(value: Boolean): CustomOptionParser[A]                  = new CustomOptionParser(optionDefaultValue,  value,           requiresAssociatedValue, transform)
  def withRequiresAssociatedValue(value: Boolean): CustomOptionParser[A]          = new CustomOptionParser(optionDefaultValue,  useDefaultValue, value,                   transform)

  def apply(value: String): Option[A] = {
    val result = transform(value)
    result match {
      case None => if (useDefaultValue) optionDefaultValue else None
      case Some(_) => result.asInstanceOf[Option[A]]
    }
  }
}
