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
object OS extends Enum {

  sealed case class EnumVal private[OS](family: OSFamily.EnumVal, platformPartName: String, variants: Seq[String] = Seq()) extends Value {
    lazy val isPOSIX = OS.isPOSIX(this)
  }

  val Unknown        = EnumVal(family = OSFamily.Unknown, platformPartName = StringUtil.empty)

  val Windows95      = EnumVal(family = OSFamily.Windows, platformPartName = "windows_95",    variants = List("Windows 95"))
  val Windows98      = EnumVal(family = OSFamily.Windows, platformPartName = "windows_98",    variants = List("Windows 98"))
  val WindowsMe      = EnumVal(family = OSFamily.Windows, platformPartName = "windows_me",    variants = List("Windows Me"))
  val WindowsNT      = EnumVal(family = OSFamily.Windows, platformPartName = "windows_nt",    variants = List("Windows NT"))
  val Windows2000    = EnumVal(family = OSFamily.Windows, platformPartName = "windows_2000",  variants = List("Windows 2000"))
  val WindowsXP      = EnumVal(family = OSFamily.Windows, platformPartName = "windows_xp",    variants = List("Windows XP"))
  val Windows2003    = EnumVal(family = OSFamily.Windows, platformPartName = "windows_2003",  variants = List("Windows 2003"))
  val Windows2008    = EnumVal(family = OSFamily.Windows, platformPartName = "windows_2008",  variants = List("Windows 2008"))
  val WindowsVista   = EnumVal(family = OSFamily.Windows, platformPartName = "windows_vista", variants = List("Windows Vista"))
  val Windows7       = EnumVal(family = OSFamily.Windows, platformPartName = "windows_7",     variants = List("Windows 7"))
  val Windows8       = EnumVal(family = OSFamily.Windows, platformPartName = "windows_8",     variants = List("Windows 8"))
  val Windows9       = EnumVal(family = OSFamily.Windows, platformPartName = "windows_9",     variants = List("Windows 9"))
  val Windows10      = EnumVal(family = OSFamily.Windows, platformPartName = "windows_10",     variants = List("Windows 10"))
  val Windows11      = EnumVal(family = OSFamily.Windows, platformPartName = "windows_11",     variants = List("Windows 11"))
  val WindowsCE      = EnumVal(family = OSFamily.Windows, platformPartName = "windows_ce",    variants = List("Windows CE"))
  val OS2            = EnumVal(family = OSFamily.Windows, platformPartName = "os_2",          variants = List("OS/2"))
  val WindowsUnknown = EnumVal(family = OSFamily.Windows, platformPartName = OSFamily.Windows.platformPartName)


  val MacOSX         = EnumVal(family = OSFamily.Mac,     platformPartName = "osx",           variants = List("Mac OS", "Mac OS X"))
  val MacUnknown     = EnumVal(family = OSFamily.Mac,     platformPartName = OSFamily.Mac.platformPartName)


  val Linux          = EnumVal(family = OSFamily.Unix,    platformPartName = "linux",         variants = List("Linux"))
  val MPE_iX         = EnumVal(family = OSFamily.Unix,    platformPartName = "mpe_ix",        variants = List("MPE/iX"))
  val HP_UX          = EnumVal(family = OSFamily.Unix,    platformPartName = "hp_ux",         variants = List("HP-UX"))
  val AIX            = EnumVal(family = OSFamily.Unix,    platformPartName = "aix",           variants = List("AIX"))
  val FreeBSD        = EnumVal(family = OSFamily.Unix,    platformPartName = "freebsd",       variants = List("FreeBSD"))
  val Irix           = EnumVal(family = OSFamily.Unix,    platformPartName = "irix",          variants = List("Irix"))
  val OS_390         = EnumVal(family = OSFamily.Unix,    platformPartName = "os390",         variants = List("OS/390"))
  val DigitalUnix    = EnumVal(family = OSFamily.Unix,    platformPartName = "digital_unix",  variants = List("Digital Unix"))
  val Netware_4_11   = EnumVal(family = OSFamily.Unix,    platformPartName = "netware_4_11",  variants = List("NetWare 4.11"))
  val OSF1           = EnumVal(family = OSFamily.Unix,    platformPartName = "osf1",          variants = List("OSF1"))
  val SunOS          = EnumVal(family = OSFamily.Unix,    platformPartName = "sunos",         variants = List("SunOS"))
  val UnixUnknown    = EnumVal(family = OSFamily.Unix,    platformPartName = OSFamily.Unix.platformPartName)


  val Solaris        = EnumVal(family = OSFamily.Solaris, platformPartName = "solaris",       variants = List("Solaris"))
  val SolarisUnknown = EnumVal(family = OSFamily.Solaris, platformPartName = OSFamily.Solaris.platformPartName)


  val VMS            = EnumVal(family = OSFamily.VMS,     platformPartName = "openvms",       variants = List("OpenVMS"))
  val VMSUnknown     = EnumVal(family = OSFamily.VMS,     platformPartName = OSFamily.VMS.platformPartName)


  def systemOSName: String = System.getProperty("os.name")
  def systemOS: EnumVal = fromName(systemOSName)
  def systemOSFamily: OSFamily.EnumVal = systemOS.family
  def isPOSIX(os: OS.EnumVal): Boolean = OSFamily.isPOSIX(os)

  def fromName(name: String): EnumVal = {
    if (name.isNullOrEmpty) {
      return Unknown
    }

    for (os <- OS.values)
      for (variant <- os.variants)
        if (variant.equalsIgnoreCase(name)) {
          return os
        }

    val lower = name.toLowerCase
    if (lower.contains("win")) WindowsUnknown
    else if (lower.contains("mac")) MacUnknown
    else if (lower.contains("nix") || lower.contains("nux")) UnixUnknown
    else if (lower.contains("vms")) VMSUnknown
    else if (lower.contains("solaris")) SolarisUnknown
    else Unknown
  }
}
