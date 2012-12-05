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

  //TODO: Use futures for getting result of parsing or waiting for parsing to complete fully...

  //TODO: Use macros to create an object representing the options (translates the option map into the results of processing it...)
  //        This will require splitting out the macro definition into a separate module. They have to be compiled separately and
  //        before the code that will use it. Please see:
  //        http://www.warski.org/blog/2012/12/starting-with-scala-macros-a-short-tutorial/

  def parse(values: String*): ParseResults = parseArguments(values)

  def parseArguments(values: Seq[String]): ParseResults = {
    if (!configuration.strategy.validateOptions(options)) {
      throw new IllegalArgumentException("The provided options do not meet the parser strategy's requirements.")
    }

    val results = configuration.strategy.processOptions(values.toStream, options)

    //Post-process results (validate required options, etc.)

    //Are there any required options that are not present?
    val any_missing_required = options.exists(p => {
      val opt = p._2._1
      opt.required && !results.contains(opt.name)
    })

    //Determine if parsing was overall successful or not.
    val success = !any_missing_required

    //Send back the results
    new ParseResults(success, results)
  }
}

class ParseResults(val success: Boolean, val optionResults: CommandLineOptionResults) {
  def apply[T](name: String): List[T] = {
    optionResults.getOrElse(name, None) match {
      case None => List()
      case Some(a) => a.asInstanceOf[List[T]]
    }
  }
}
