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

package scalaopts.common.platform

import scalaopts.common.OSFamily
import scalaopts.common.OSFamily._

/**
 */
object Terminal extends ITerminal {
  val instance: ITerminal =
    OSFamily.systemOSFamily match {
      case Windows => new Win32.Terminal()
      case _ => ITerminal.Instance
    }

  def queryTerminalDimensions(): ITerminal.Dimension = instance.queryTerminalDimensions()
}
