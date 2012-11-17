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

import StringUtil._

/**
 * Gathers information about operating systems and the one we're hosted on.
 *
 * @author David Hoyt <dhoyt@hoytsoft.org>
 */
object OSFamily extends Enum {

  sealed case class EnumVal private[OSFamily](platformPartName: String, isPosix: Boolean) extends Value

  val Unknown = EnumVal(platformPartName = StringUtil.empty, isPosix = false)
  val Windows = EnumVal(platformPartName = "windows",        isPosix = false)
  val Mac     = EnumVal(platformPartName = "osx",            isPosix = true)
  val Unix    = EnumVal(platformPartName = "unix",           isPosix = true)
  val Solaris = EnumVal(platformPartName = "solaris",        isPosix = true)
  val VMS     = EnumVal(platformPartName = "vms",            isPosix = false)

  def isPOSIX(family: EnumVal): Boolean = family.isPosix
  def isPOSIX(os: OS.EnumVal): Boolean = isPOSIX(os.family)

  def systemOS: OS.EnumVal = OS.systemOS
  def systemOSFamily: EnumVal = OS.systemOSFamily

  def fromName(name: String): EnumVal = {
    if (name.isNullOrEmpty) {
      return Unknown
    }

    for(family <- OSFamily.values)
      if (family.platformPartName.equalsIgnoreCase(name)) {
        return family
      }

    val lower = name.toLowerCase
    if (lower.contains("win")) Windows
    else if (lower.contains("mac")) Mac
    else if (lower.contains("nix") || lower.contains("nux")) Unix
    else if (lower.contains("vms")) VMS
    else if (lower.contains("solaris")) Solaris
    else Unknown
  }
}
