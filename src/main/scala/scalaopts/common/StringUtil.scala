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

/**
 * Extensions for [[java.lang.String]] classes.
 */
object StringUtil {
  implicit final def extend(s: String): StringExtensions = StringExtensions(s)

  val empty = ""

  /** Returns if the string is either null or empty. */
  def isNullOrEmpty(s: String): Boolean = (s == null /* ignore style check */ || s.isEmpty || empty == s)

  /** Returns if the string is not null and not empty. */
  def isNonEmpty(s: String): Boolean = !isNullOrEmpty(s)

  /** Returns the string if it is not null and not empty, otherwise the empty string. */
  def checked(s: String): String = if (isNullOrEmpty(s)) empty else s

  @inline case class StringExtensions(s: String) {
    /** @see [[scalaopts.common.StringUtil.isNullOrEmpty()]] */
    def isNullOrEmpty: Boolean = StringUtil.isNullOrEmpty(s)

    /** @see [[scalaopts.common.StringUtil.isNonEmpty()]] */
    def isNonEmpty: Boolean = StringUtil.isNonEmpty(s)

    /** @see [[scalaopts.common.StringUtil.checked()]] */
    def checked: String = StringUtil.checked(s)
  }
}




