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

object ParserTransforms {
  def createParserMap(options: Seq[TypedCommandLineOption[_]]): CommandLineOptionMap = {
    //Ensure that we have a set of unique names across option names
    val option_names_with_potential_duplicates = options.map(_.name)
    val unique_option_names = option_names_with_potential_duplicates.distinct
    val non_unique_option_names = option_names_with_potential_duplicates.diff(unique_option_names)
    if (!non_unique_option_names.isEmpty) {
      throw new IllegalArgumentException("Command line options must have unique names across the entire set. The following are non-unique names: " + (non_unique_option_names mkString ", "))
    }

    //Ensure that we have a set of unique names across option long names and short names
    val names_with_potential_duplicates = options.map(_.longNames).flatten ++ options.map(_.shortNames).flatten
    val unique_names = names_with_potential_duplicates.distinct
    val non_unique_names = names_with_potential_duplicates.diff(unique_names)
    if (!non_unique_names.isEmpty) {
      throw new IllegalArgumentException("Command line options must have unique long names and short names across the entire set. The following are non-unique names: " + (non_unique_names mkString ", "))
    }

    (
      for {
        opt <- options
      }
        yield opt.name -> opt
    ).toMap
  }

  def createParser(configuration: ParserConfiguration, options: Seq[TypedCommandLineOption[_]]): Parser = {
    new Parser(configuration, createParserMap(options))
  }
}
