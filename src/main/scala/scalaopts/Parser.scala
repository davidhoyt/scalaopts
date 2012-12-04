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

import common.StringUtil._
import annotation.tailrec

class Parser(val configuration: ParserConfiguration, val options: CommandLineOptionMap) {

  def parse(values: String*): Boolean = parseArguments(values)

  //We don't really want to return a boolean - that's just a placeholder for now
  def parseArguments(values: Seq[String]): Boolean = {
    if (!configuration.strategy.validateOptions(options)) {
      throw new IllegalArgumentException("The provided options do not meet the parser strategy's requirements.")
    }

    configuration.strategy.processOptions(values.toStream, options)
    true
    //parse0(values.map(s => s.trim), None)
  }

  //TODO: Use futures for getting result of parsing or waiting for parsing to complete fully...

  //TODO: Use macros to create an object representing the options (translates the option map into the results of processing it...)
  //        This will require splitting out the macro definition into a separate module. They have to be compiled separately and
  //        before the code that will use it. Please see:
  //        http://www.warski.org/blog/2012/12/starting-with-scala-macros-a-short-tutorial/

  //We don't really want to return a boolean - that's just a placeholder for now
  @tailrec
  private def parse0(values: Seq[String], current_param: Option[String]): Boolean = values match {
    case Nil => {
      true
    }
    case Seq(value, tail @_*) => {
      value match {
//        case Seq(first_char, param_name @_*) if first_char == configuration.simpleArgumentPattern => {
//          parse0(tail, Some(param_name))
//        }
        case _ => {
          parse0(tail, current_param)
        }
      }
    }
  }

  //def process
}
