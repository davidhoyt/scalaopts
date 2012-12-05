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

/**
 * Defines common transforms from a [[java.lang.String]] to a value.
 */
object OptionParserStringTransforms {
  /**
   * Function that accepts a string and changes it to a value of type A.
   *
   * @tparam A Result type of applying the function to the given [[java.lang.String]] argument.
   */
  type FnTransform[_] = String => Option[_]

  val TRANSFORM_BOOLEAN: FnTransform[Boolean] = s => try { Some(s.toBoolean)                                } catch { case _: Throwable => None }
  val TRANSFORM_CHAR:    FnTransform[Char]    = s => try { s.toCharArray.headOption                         } catch { case _: Throwable => None }
  val TRANSFORM_BYTE:    FnTransform[Byte]    = s => try { Some(s.toByte)                                   } catch { case _: Throwable => None }
  val TRANSFORM_SHORT:   FnTransform[Short]   = s => try { Some(s.toShort)                                  } catch { case _: Throwable => None }
  val TRANSFORM_INT:     FnTransform[Int]     = s => try { Some(s.toInt)                                    } catch { case _: Throwable => None }
  val TRANSFORM_LONG:    FnTransform[Long]    = s => try { Some(s.toLong)                                   } catch { case _: Throwable => None }
  val TRANSFORM_FLOAT:   FnTransform[Float]   = s => try { Some(s.toFloat)                                  } catch { case _: Throwable => None }
  val TRANSFORM_DOUBLE:  FnTransform[Double]  = s => try { Some(s.toDouble)                                 } catch { case _: Throwable => None }
  val TRANSFORM_STRING:  FnTransform[String]  = s => try { if (StringUtil.isNonEmpty(s)) Some(s) else None  } catch { case _: Throwable => None }
  val TRANSFORM_FILE:    FnTransform[File]    = s => try { Some(new File(s))                                } catch { case _: Throwable => None }
  val TRANSFORM_NOOP:    FnTransform[Boolean] = s => Some(true)
}
