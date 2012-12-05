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

package scalaopts.common

import annotation.tailrec

/**
 * Setup for emulating C#'s default keyword. Please see:
 *   http://missingfaktor.blogspot.com/2011/08/emulating-cs-default-keyword-in-scala.html
 */
trait Default[A] {
  def value: A
}

trait LowPriorityImplicitsForDefault { this: Default.type =>
  implicit def forAnyRef[A](implicit ev: Null <:< A) = Default withValue (null : A)
}

object Default extends LowPriorityImplicitsForDefault {
  def withValue[A](a: A) = new Default[A] {
    def value = a
  }

  implicit val forBoolean = Default withValue false
  implicit val forChar = Default withValue ' '
  implicit def forNumeric[A](implicit n: Numeric[A]) = Default withValue n.zero
  implicit val forString = Default withValue ""
  implicit def forOption[A] = Default withValue (None: Option[A])
}
