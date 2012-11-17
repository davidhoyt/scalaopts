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

/** Provides classes for command line argument parsing.
  * Also provides implicits for [[java.lang.String]] processing.
  *
  * ==Overview==
  * Begin by defining options. For example:
  * {{{
  *   options {
  *     opt: "v" %% "verbose" %% "description" %% BooleanOpt/FlagOpt
  *     opt: "p" %% "print" %% "print description" %% BooleanOpt
  *   }
  *   option named "verbose" alias "v" alias "q" describedAs "description" parseAs Boolean,
  *   option named "print" alias "p" describedAs "print description" parseAs Boolean
  * }}}
 */
package object scalaopts {
  object CommandLineOptions {
    object DEFAULT_PARSER_CONFIGURATION extends ParserConfiguration(
      argumentNameSeparator = '-'
    )

    def apply(args: TypedCommandLineOption[_]*): Parser = applySeq(args)
    def apply(configuration: ParserConfiguration)(args: TypedCommandLineOption[_]*): Parser = applySeq(configuration)(args)

    def applySeq(args: Seq[TypedCommandLineOption[_]]): Parser = applySeq(DEFAULT_PARSER_CONFIGURATION)(args)
    def applySeq(configuration: ParserConfiguration)(args: Seq[TypedCommandLineOption[_]]): Parser = {
      ParserTransforms.createParser(configuration, args)
    }
  }
}
