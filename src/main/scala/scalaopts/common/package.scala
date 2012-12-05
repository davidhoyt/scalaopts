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

/**
 */
package object common {
  /**
   * Provides functionality similar to C#'s default keyword.
   * However, default is now pimped -- you can do much more with it
   * than you can C#'s.
   */
  def default[A: Default] = implicitly[Default[A]].value

  /**
   * Provides functionality similar to C#'s default keyword.
   * Use when default[A] doesn't work.
   *
   * Alternative is to use "null.asInstanceOf[A]" which will
   * accomplish the same task.
   */
  def defaultValue[A] = {
    class Temp {
      var default_value: A = _
    }
    (new Temp).default_value
  }
}
