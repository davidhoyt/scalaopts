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

import com.dongxiguo.zeroLog.formatters.SimpleFormatter
import com.dongxiguo.zeroLog.Filter
import com.dongxiguo.zeroLog.appenders.ConsoleAppender
import strategy.GNUParserStrategy

/**
 * Commands describe available directives that will be used when actually parsing command line
 * arguments down the line.
 */
object ZeroLoggerFactory {
  def isAssignableFrom(runtimeClass: Class[_])(cls: Class[_]): Boolean = runtimeClass.isAssignableFrom(cls)

  final def newLogger[T](implicit desired:Manifest[T]) = {
    val cls = desired.runtimeClass
    val is = isAssignableFrom(cls)_

    cls match {
      case _ if is(classOf[GNUParserStrategy]) => (Filter.Finest, SimpleFormatter, ConsoleAppender)
      case _ => (Filter.Finest, SimpleFormatter, ConsoleAppender)
    }
  }
}
