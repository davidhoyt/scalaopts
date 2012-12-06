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

import common.{StringUtil, Default}

/** The starting point of the DSL from which we will build an argument. */
case object CommandLineOption extends Command {
  def named(name: String): CommandLineOptionStep2[String] = new CommandLineOptionStep2(
      name = name
    , defaultValue = Some(StringUtil.empty)
    , parser = Some(StringOption(defaultValue = StringUtil.empty))
  )

  //TODO: Split this out into multiple steps to ensure that all is_required information is captured in order and can be checked by the compiler.
  //TODO: Create CommandLineFlag version of CommandLineOption that sets up appropriate option parser, etc.
  //TODO: Default parser/accumulator should be StringOption/StringList

  /**
   * Used as a builder to describe an option.
   *
   * @param name The full name of the command line argument.
   * @param description A description used in usage and help text.
   * @param parser What is intended to convert the argument into a typed value.
   */
  class CommandLineOptionStep2[+A](
      name:                 String
    , is_required:          Boolean                 = false
    , longNames:            List[String]            = List()
    , shortNames:           List[String]            = List()
    , dependencies:         List[String]            = List()
    , description:          String                  = ""
    , arity:                Int                     = 1
    , minNumberOfArguments: Int                     = 1
    , maxNumberOfArguments: Int                     = 1
    , defaultValue:         Option[A]               = None
    , parser:               Option[OptionParser[A]] = None
  ) extends Command {
    def required:                                       CommandLineOptionStep2[A] = new CommandLineOptionStep2(name, true,        longNames,          shortNames,          dependencies,             description, arity, minNumberOfArguments, maxNumberOfArguments, defaultValue, parser)
    def notRequired:                                    CommandLineOptionStep2[A] = new CommandLineOptionStep2(name, false,       longNames,          shortNames,          dependencies,             description, arity, minNumberOfArguments, maxNumberOfArguments, defaultValue, parser)
    def required(value: Boolean):                       CommandLineOptionStep2[A] = new CommandLineOptionStep2(name, value,       longNames,          shortNames,          dependencies,             description, arity, minNumberOfArguments, maxNumberOfArguments, defaultValue, parser)
    def shortName(value: String):                       CommandLineOptionStep2[A] = new CommandLineOptionStep2(name, is_required, longNames,          value :: shortNames, dependencies,             description, arity, minNumberOfArguments, maxNumberOfArguments, defaultValue, parser)
    def longName(value: String):                        CommandLineOptionStep2[A] = new CommandLineOptionStep2(name, is_required, value :: longNames, shortNames,          dependencies,             description, arity, minNumberOfArguments, maxNumberOfArguments, defaultValue, parser)
    def arity(value: Int):                              CommandLineOptionStep2[A] = new CommandLineOptionStep2(name, is_required, longNames,          shortNames,          dependencies,             description, value, minNumberOfArguments, maxNumberOfArguments, defaultValue, parser)
    def numberOfArguments(value: Int):                  CommandLineOptionStep2[A] = new CommandLineOptionStep2(name, is_required, longNames,          shortNames,          dependencies,             description, arity, value,                value,                defaultValue, parser)
    def minNumberOfArguments(value: Int):               CommandLineOptionStep2[A] = new CommandLineOptionStep2(name, is_required, longNames,          shortNames,          dependencies,             description, arity, value,                maxNumberOfArguments, defaultValue, parser)
    def maxNumberOfArguments(value: Int):               CommandLineOptionStep2[A] = new CommandLineOptionStep2(name, is_required, longNames,          shortNames,          dependencies,             description, arity, minNumberOfArguments, value,                defaultValue, parser)
    def flag:                                           CommandLineOptionStep2[A] = new CommandLineOptionStep2(name, is_required, longNames,          shortNames,          dependencies,             description, arity, 0,                    0,                    defaultValue, parser)
    def describedAs(value: String):                     CommandLineOptionStep2[A] = new CommandLineOptionStep2(name, is_required, longNames,          shortNames,          dependencies,             value,       arity, minNumberOfArguments, maxNumberOfArguments, defaultValue, parser)
    def dependsOn(value: String):                       CommandLineOptionStep2[A] = new CommandLineOptionStep2(name, is_required, longNames,          shortNames,          value :: dependencies,    description, arity, minNumberOfArguments, maxNumberOfArguments, defaultValue, parser)
    def dependsOn(opt: CommandLineOptionMapTypedValue): CommandLineOptionStep2[A] = new CommandLineOptionStep2(name, is_required, longNames,          shortNames,          opt.name :: dependencies, description, arity, minNumberOfArguments, maxNumberOfArguments, defaultValue, parser)
    def arguments(min: Int, max: Int):                  CommandLineOptionStep2[A] = new CommandLineOptionStep2(name, is_required, longNames,          shortNames,          dependencies,             description, arity, min,                  max,                  defaultValue, parser)
    def arguments(value: Int):                          CommandLineOptionStep2[A] = numberOfArguments(value)
    def arguments(range: Range):                        CommandLineOptionStep2[A] = arguments(range.start, range.end)
    def flag(value: Boolean):                           CommandLineOptionStep2[A] = if (value) { flag } else { this }
    def default[T](value: T):                           CommandLineOptionStep3[T] = new CommandLineOptionStep3[T](name, is_required, if (longNames.isEmpty) List(name) else longNames, shortNames, dependencies, description, arity, minNumberOfArguments, maxNumberOfArguments, Some(value), None)
    def parseAs[T](value: OptionParser[T]):             CommandLineOptionStep3[T] = new CommandLineOptionStep3[T](name, is_required, if (longNames.isEmpty) List(name) else longNames, shortNames, dependencies, description, arity, minNumberOfArguments, maxNumberOfArguments, None, Some(value))
  }

  //class CommandLineOptionStepEx[+A]()
  //  extends

  class CommandLineOptionStep3[+A](
      val name:                 String
    , val required:             Boolean
    , val longNames:            List[String]
    , val shortNames:           List[String]
    , val dependencies:         List[String]
    , val description:          String
    , val arity:                Int
    , val minNumberOfArguments: Int
    , val maxNumberOfArguments: Int
    , val defaultValue:         Option[A]
    , val parser:               Option[OptionParser[A]]
  ) extends MinimumTypedCommandLineOption[A] {
    def default[T >: A](value: T): CommandLineOptionStep3[T] =
      new CommandLineOptionStep3(name, required, longNames, shortNames, dependencies, description, arity, minNumberOfArguments, maxNumberOfArguments, Some(value), parser)
    def parseAs[T >: A](value: OptionParser[T]): CommandLineOptionStep3[T] =
      new CommandLineOptionStep3(name, required, longNames, shortNames, dependencies, description, arity, minNumberOfArguments, maxNumberOfArguments, defaultValue, Some(value))

    def accumulateWith[X >: A, B, C](value: OptionArgumentAccumulator[X, B, C]): TypedCommandLineOption[X, B, C] =
      accumulateBy(value)
    def accumulateBy[X >: A, B, C](value: OptionArgumentAccumulator[X, B, C]): TypedCommandLineOption[X, B, C] =
      new FinalTypedCommandLineOption(name, required, longNames, shortNames, dependencies, description, arity, minNumberOfArguments, maxNumberOfArguments, defaultValue, parser.get, value)
  }

  class FinalTypedCommandLineOption[A, +B, +C] (
      val name: String
    , val required: Boolean
    , val longNames: List[String]
    , val shortNames: List[String]
    , val dependencies: List[String]
    , val description: String
    , val arity: Int
    , val minNumberOfArguments: Int
    , val maxNumberOfArguments: Int
    , val defaultValue: Option[A]
    , val parser: OptionParser[A]
    , val accumulator: OptionArgumentAccumulator[A, B, C]
  ) extends TypedCommandLineOption[A, B, C]

  def toTypedCommandLineOption[A: Default](value: CommandLineOptionStep3[A]): TypedCommandLineOption[A, _, _] = {
    if (value.parser.isEmpty) {
      throw new IllegalArgumentException("Parser has not been defined. Please specify the OptionParser via the parseAs() method.")
    }

    if (value.isSingleArgument) {
      new FinalTypedCommandLineOption(value.name, value.required, value.longNames, value.shortNames, value.dependencies, value.description, value.arity, value.minNumberOfArguments, value.maxNumberOfArguments, value.defaultValue, value.parser.get, SingleOptionArgumentAccumulator[A]())
    } else {
      new FinalTypedCommandLineOption(value.name, value.required, value.longNames, value.shortNames, value.dependencies, value.description, value.arity, value.minNumberOfArguments, value.maxNumberOfArguments, value.defaultValue, value.parser.get, new ListOptionArgumentAccumulator[A]())
    }
  }
}

case object CommandLineFlag extends Command {
  def named(name: String): CommandLineOption.CommandLineOptionStep3[Boolean] =
    new CommandLineOption.CommandLineOptionStep2(
        name = name
      , defaultValue = Some(false)
    )
    .flag
    .parseAs(BooleanOption(defaultValue = false))
}

sealed trait MinimumTypedCommandLineOption[+A] {
  def name:                 String
  def required:             Boolean
  def longNames:            List[String]
  def shortNames:           List[String]
  def dependencies:         List[String]
  def description:          String
  def arity:                Int
  def minNumberOfArguments: Int
  def maxNumberOfArguments: Int

  def isFlag: Boolean = isMinNumberOfArgumentsUnbounded || minNumberOfArguments == 0
  def isRequired: Boolean = required
  def isSingleArgument: Boolean = !isMinNumberOfArgumentsUnbounded && !isMaxNumberOfArgumentsUnbounded && minNumberOfArguments == 1 && maxNumberOfArguments == 1
  def isArityUnbounded: Boolean = arity == UNBOUNDED
  def isMinNumberOfArgumentsUnbounded: Boolean = minNumberOfArguments == UNBOUNDED
  def isMaxNumberOfArgumentsUnbounded: Boolean = maxNumberOfArguments == UNBOUNDED
  def isMatchForLongName(name_to_match: String): Boolean = longNames.find(_.equalsIgnoreCase(name_to_match)).isDefined
  def isMatchForShortName(name_to_match: String): Boolean = shortNames.find(_.equalsIgnoreCase(name_to_match)).isDefined
  def isMatchForName(name_to_match: String): Boolean = isMatchForLongName(name_to_match) || isMatchForShortName(name_to_match)

  if (!isMinNumberOfArgumentsUnbounded && !isMaxNumberOfArgumentsUnbounded && maxNumberOfArguments < minNumberOfArguments) {
    throw new IllegalArgumentException("maxNumberOfArguments must be >= minNumberOfArguments")
  }

  if (!isArityUnbounded && arity < 1) {
    throw new IllegalArgumentException("arity must be >= 1 or specified as unbounded")
  }
}

sealed trait TypedCommandLineOption[+A, +B, +C] extends MinimumTypedCommandLineOption[A] {
  def defaultValue: Option[A]
  def parser:       OptionParser[A]
  def accumulator:  OptionArgumentAccumulator[A, B, C]

  def apply(value: String): Option[A] =
    parser(value)
}
