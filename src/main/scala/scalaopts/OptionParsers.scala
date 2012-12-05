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

import _root_.java.io.File
import common.StringUtil
import OptionParserStringTransforms._

case class ByteOption(defaultValue: Byte = 0) extends CustomOptionParser[Byte](Some(defaultValue), true, true, TRANSFORM_BYTE)
object DefaultByteOption extends ByteOption()

case class ShortOption(defaultValue: Short = 0) extends CustomOptionParser[Short](Some(defaultValue), true, true, TRANSFORM_SHORT)
object DefaultShortOption extends ShortOption()

case class IntegerOption(defaultValue: Int = 0) extends CustomOptionParser[Int](Some(defaultValue), true, true, TRANSFORM_INT)
object DefaultIntegerOption extends IntegerOption()

case class LongOption(defaultValue: Long = 0L) extends CustomOptionParser[Long](Some(defaultValue), true, true, TRANSFORM_LONG)
object DefaultLongOption extends LongOption()

case class FloatOption(defaultValue: Float = 0.0f) extends CustomOptionParser[Float](Some(defaultValue), true, true, TRANSFORM_FLOAT)
object DefaultFloatOption extends FloatOption()

case class DoubleOption(defaultValue: Double = 0.0D) extends CustomOptionParser[Double](Some(defaultValue), true, true, TRANSFORM_DOUBLE)
object DefaultDoubleOption extends DoubleOption()

case class BooleanOption(defaultValue: Boolean = false) extends CustomOptionParser[Boolean](Some(defaultValue), true, true, TRANSFORM_BOOLEAN)
object DefaultBooleanOption extends BooleanOption()

case class FlagOption() extends CustomOptionParser[Boolean](Some(true), true, false, TRANSFORM_NOOP)
object DefaultFlagOption extends FlagOption()

case class CharOption(defaultValue: Char = '\0') extends CustomOptionParser[Char](Some(defaultValue), true, true, TRANSFORM_CHAR)
object DefaultCharOption extends CharOption()

case class StringOption(defaultValue: String = StringUtil.empty) extends CustomOptionParser[String](Some(defaultValue), true, true, TRANSFORM_STRING)
object DefaultStringOption extends StringOption()

case class FileOption(defaultValue: Option[File] = None) extends CustomOptionParser[File](defaultValue, true, true, TRANSFORM_FILE)
object DefaultFileOption extends FileOption()
