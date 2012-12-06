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

class Parser(val configuration: ParserConfiguration, val options: CommandLineOptionMap) {

  //TODO: Use futures for getting result of parsing or waiting for parsing to complete fully...

  //TODO: Use macros to create an object representing the options (translates the parse result into an option where field names are option names...)
  //        This will require splitting out the macro definition into a separate module. They have to be compiled separately and
  //        before the code that will use it. Please see:
  //        http://www.warski.org/blog/2012/12/starting-with-scala-macros-a-short-tutorial/

  def parse(values: String*): ParseResults = parseArguments(values)

  def parseArguments(values: Seq[String]): ParseResults = {
    if (!configuration.strategy.validateOptions(options)) {
      throw new IllegalArgumentException("The provided options do not meet the parser strategy's requirements.")
    }

    val results = configuration.strategy.processOptions(values.toStream, options)

    //Takes something like:
    //  size -> Some(List(List(1, 2, 3)))
    //and converts it to:
    //  size -> List(List(1, 2, 3))
    val processed_results = results.map(m => m._1 -> m._2.get)

    //Post-process results (validate required options, etc.)

    //Are there any required options that are not present?
    val any_missing_required = options.exists(p => {
      val opt = p._2._1
      opt.required && !processed_results.contains(opt.name)
    })

    //Determine if parsing was overall successful or not.
    val success = !any_missing_required

    //Send back the results
    new ParseResults(success, processed_results, options)
  }
}

class ParseResults(val success: Boolean, val optionResults: CommandLineOptionParseResults, val options: CommandLineOptionMap) {
  def apply[T](name: String): Option[Seq[T]] = optionResults.get(name) match {
    case None => None
    case Some(value) => Some(value.asInstanceOf[Seq[T]])
  }

  def find[T](name: String): Option[Seq[T]] =
    apply(name)

  def first[T](name: String): Option[T] = find(name) match {
    case Some(value) if !value.isEmpty => Some(value.head)
    case Some(value) => options.get(name) match {
      case None => None
      case Some(original) => original._1.defaultValue.asInstanceOf[Option[T]]
    }
    case None => None
    case _ => None
  }
}
