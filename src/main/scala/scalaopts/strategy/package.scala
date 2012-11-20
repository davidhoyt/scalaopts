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

import scalaopts.common.StringUtil._
import annotation.tailrec

/**
 */
package object strategy {

  def findMatchingCommandLineOption(command_line_options: CommandLineOptionMap)(name: String): Option[CommandLineOptionMapValue] =
    findMatchingMapValue(command_line_options)(name)((value, looking_for) => value.isMatchForName(looking_for))

  def findMatchingCommandLineOptionByLongName(command_line_options: CommandLineOptionMap)(name: String): Option[CommandLineOptionMapValue] =
    findMatchingMapValue(command_line_options)(name)((value, looking_for) => value.isMatchForLongName(looking_for))

  def findMatchingCommandLineOptionByShortName(command_line_options: CommandLineOptionMap)(name: String): Option[CommandLineOptionMapValue] =
    findMatchingMapValue(command_line_options)(name)((value, looking_for) => value.isMatchForShortName(looking_for))

  /**
   * Return the value in a map matching a given predicate p.
   *
   * @return The value matching the predicate p.
   */
  def findMatchingMapValue[A, B, C](m: Map[A, B])(looking_for: C)(p: (B, C) => Boolean): Option[B] = m.find(t => p(t._2, looking_for)) match {
    case None => None
    case Some((key, value)) => Some(value)
  }

  def stripLeadingCharacter(c: Char)(s: String): String =
    s.dropWhile(_ == c)

  def splitAtCharacter(c: Char)(s: String): (String, String, Boolean) = {
    val index = s.indexOf(c)
    if (index >= 0) (s.take(index), s.drop(index + 1), true)
    else (s, empty, false)
  }

  val stripLeadingHyphens = stripLeadingCharacter('-')_
  val splitAtEquals = splitAtCharacter('=')_
}
