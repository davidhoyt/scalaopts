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
 * Extensions for [[java.lang.String]] classes.
 */
object StringUtil {
  implicit final def extend(s: String): StringExtensions = StringExtensions(s)

  val empty = ""

  /** Returns if the string is either null or empty. */
  @inline def isNullOrEmpty(s: String): Boolean = (s == null /* ignore style check */ || s.isEmpty || empty == s)

  /** Returns if the string is not null and not empty. */
  @inline def isNonEmpty(s: String): Boolean = !isNullOrEmpty(s)

  /** Returns the string if it is not null and not empty, otherwise the empty string. */
  @inline def checked(s: String): String = if (isNullOrEmpty(s)) empty else s

  /** Returns a string that represents a class' package name and it's simple name appended with a period (.). */
  def toKeyPrefix(c: Class[_]): String = {
    val package_name = c.getPackage.getName
    val simple_name = c.getSimpleName
    val name = if (simple_name.endsWith("$")) simple_name.substring(0, simple_name.length - "$".length) else simple_name
    package_name + "." + name + "."
  }

  /** Returns the string as a valid Java/Scala identifier. */
  def toValidIdentifier(s: String): String = {

    def toValidIdentifier0(s: String): String =
      s.map(c => if (isValidChar(c)) c else '_')

    def isValidChar(c: Char): Boolean =
      Character.isLetterOrDigit(c) || c == '_' || c == '$'

    if (isNonEmpty(s)) {
      if (Character.isLetter(s.head)) {
        toValidIdentifier0(s)
      } else if (Character.isDigit(s.head)) {
        "_" + toValidIdentifier0(s)
      } else {
        toValidIdentifier0(s)
      }
    } else {
      "_"
    }
  }

  @inline case class StringExtensions(s: String) {
    /** @see [[scalaopts.common.StringUtil.isNullOrEmpty()]] */
    def isNullOrEmpty: Boolean = StringUtil.isNullOrEmpty(s)

    /** @see [[scalaopts.common.StringUtil.isNonEmpty()]] */
    def isNonEmpty: Boolean = StringUtil.isNonEmpty(s)

    /** @see [[scalaopts.common.StringUtil.checked()]] */
    def checked: String = StringUtil.checked(s)

    /** @see [[scalaopts.common.StringUtil.toValidIdentifier()]] */
    def toValidIdentifier: String = StringUtil.toValidIdentifier(s)
  }
}




