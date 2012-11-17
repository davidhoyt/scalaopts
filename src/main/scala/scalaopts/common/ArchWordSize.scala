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
 * Describes various processor architectures.
 *
 * @author David Hoyt <dhoyt@hoytsoft.org>
 */
object ArchWordSize extends Enum {
  sealed case class EnumVal private[ArchWordSize](title: String) extends Value

  val Unknown     = EnumVal("Unknown")
  val Size8Bits   = EnumVal("Size8Bits")
  val Size16Bits  = EnumVal("Size16Bits")
  val Size32Bits  = EnumVal("Size32Bits")
  val Size64Bits  = EnumVal("Size64Bits")
  val Size128Bits = EnumVal("Size128Bits")
  val Size256Bits = EnumVal("Size256Bits")
  val Size512Bits = EnumVal("Size512Bits")
}
