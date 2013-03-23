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

import language.implicitConversions

import scalaopts.common.Default
import scalaopts.strategy.GNUParserStrategy

/** Provides classes for command line argument parsing.
  * Also provides implicits for String processing.
  *
  * ==Overview==
  * Begin by defining command_line_options. For example:
  * {{{
  *   command_line_options {
  *     opt: "v" %% "verbose" %% "description" %% BooleanOpt/FlagOpt
  *     opt: "p" %% "print" %% "print description" %% BooleanOpt
  *   }
  *   option named "verbose" shortName "v" shortName "q" describedAs "description" parseAs Boolean,
  *   option named "print" shortName "p" describedAs "print description" parseAs Boolean
  * }}}
 */
package object scalaopts {
  val UNBOUNDED = -1
  val YES = true
  val NO = false

  type CommandLineOptionMapKey           = String
  type CommandLineOptionMapTypedValue    = TypedCommandLineOption[_, _, _]
  type CommandLineOptionMapValue         = (CommandLineOptionMapTypedValue, _)
  type CommandLineOptionMap              = Map[CommandLineOptionMapKey, CommandLineOptionMapValue]

  type CommandLineOptionResultKey        = CommandLineOptionMapKey
  type CommandLineOptionResultTypedValue = CommandLineOptionMapTypedValue
  type CommandLineOptionResultValue      = Option[List[_]]
  type CommandLineOptionResults          = Map[CommandLineOptionResultKey, CommandLineOptionResultValue]

  type CommandLineOptionParseResultKey   = String
  type CommandLineOptionParseResultValue = List[Any]
  type CommandLineOptionParseResults     = Map[CommandLineOptionParseResultKey, CommandLineOptionParseResultValue]
  type CommandLineOptionParseErrors      = Map[ParserError.EnumVal, Any]

  type CommandLineSpecification          = Parser

  object CommandLineOptions {
    object DEFAULT_PARSER_CONFIGURATION extends ParserConfiguration(
      strategy = new GNUParserStrategy()
    )

    def apply(args: CommandLineOptionMapTypedValue*): CommandLineSpecification = applySeq(args)
    def apply(configuration: ParserConfiguration)(args: CommandLineOptionMapTypedValue*): CommandLineSpecification = applySeq(configuration)(args)

    def applySeq(args: Seq[CommandLineOptionMapTypedValue]): CommandLineSpecification = applySeq(DEFAULT_PARSER_CONFIGURATION)(args)
    def applySeq(configuration: ParserConfiguration)(args: Seq[CommandLineOptionMapTypedValue]): CommandLineSpecification = {
      ParserTransforms.createParser(configuration, args)
    }
  }

  implicit def step2ToFinal[A: Default](value: CommandLineOption.CommandLineOptionStep2[A]): TypedCommandLineOption[A, _, _] =
    CommandLineOption.toTypedCommandLineOption(value)

  implicit def step3ToFinal[A: Default](value: CommandLineOption.CommandLineOptionStep3[A]): TypedCommandLineOption[A, _, _] =
    CommandLineOption.toTypedCommandLineOption(value)
}
