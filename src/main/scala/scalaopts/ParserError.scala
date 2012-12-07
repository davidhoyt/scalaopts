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

import common.{StringUtil, Enum}

/**
 * Describes various parser errors.
 *
 * @author David Hoyt <dhoyt@hoytsoft.org>
 */
object ParserError extends Enum {
  sealed case class EnumVal private[ParserError](code: Int, title: String, key: String, message: String) extends Value

  private val KEY_PREFIX = StringUtil.toKeyPrefix(getClass)

  val Unknown             = EnumVal( 0, "Unknown",              KEY_PREFIX + "Unknown",             "An unknown error has occurred")
  val MissingRequired     = EnumVal( 1, "Missing Required",     KEY_PREFIX + "MissingRequired",     "Required options are missing")
  val MissingDependencies = EnumVal( 2, "Missing Dependencies", KEY_PREFIX + "MissingDependencies", "One or more required dependencies are missing")
}
