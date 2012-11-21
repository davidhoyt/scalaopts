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

/** The starting point of the DSL from which we will build an argument. */
case object CommandLineOption extends Command {
  def named[A](name: String): CommandLineOptionStep2[A] = CommandLineOptionStep2(name = name)

  //TODO: Split this out into multiple steps to ensure that all is_required information is captured in order and can be checked by the compiler.
  //TODO: Create CommandLineFlag version of CommandLineOption that sets up appropriate option parser, etc.
  /**
   * Used as a builder to describe an option.
   *
   * @param name The full name of the command line argument.
   * @param description A description used in usage and help text.
   * @param parser What is intended to convert the argument into a typed value.
   */
  case class CommandLineOptionStep2[A](name: String, is_required: Boolean = false, longNames: List[String] = List(), shortNames: List[String] = List(), dependencies: List[String] = List(), description: String = "", arity: Int = 1, minNumberOfRequiredValues: Int = 1, maxNumberOfRequiredValues: Int = 1, parser: Option[OptionParser[A]] = None) extends Command {
    def required: CommandLineOptionStep2[A]                                     = CommandLineOptionStep2(name, true,        longNames,          shortNames,          dependencies,             description, arity, minNumberOfRequiredValues, maxNumberOfRequiredValues, parser)
    def notRequired: CommandLineOptionStep2[A]                                  = CommandLineOptionStep2(name, false,       longNames,          shortNames,          dependencies,             description, arity, minNumberOfRequiredValues, maxNumberOfRequiredValues, parser)
    def required(value: Boolean): CommandLineOptionStep2[A]                     = CommandLineOptionStep2(name, value,        longNames,          shortNames,          dependencies,             description, arity, minNumberOfRequiredValues, maxNumberOfRequiredValues, parser)
    def shortName(value: String): CommandLineOptionStep2[A]                     = CommandLineOptionStep2(name, is_required, longNames,          value :: shortNames, dependencies,             description, arity, minNumberOfRequiredValues, maxNumberOfRequiredValues, parser)
    def longName(value: String): CommandLineOptionStep2[A]                      = CommandLineOptionStep2(name, is_required, value :: longNames, shortNames,          dependencies,             description, arity, minNumberOfRequiredValues, maxNumberOfRequiredValues, parser)
    def arity(value: Int): CommandLineOptionStep2[A]                            = CommandLineOptionStep2(name, is_required, longNames,          shortNames,          dependencies,             description, value, minNumberOfRequiredValues, maxNumberOfRequiredValues, parser)
    def numberOfRequiredValues(value: Int): CommandLineOptionStep2[A]           = CommandLineOptionStep2(name, is_required, longNames,          shortNames,          dependencies,             description, arity, value,                     value,                     parser)
    def minNumberOfRequiredValues(value: Int): CommandLineOptionStep2[A]        = CommandLineOptionStep2(name, is_required, longNames,          shortNames,          dependencies,             description, arity, value,                     maxNumberOfRequiredValues, parser)
    def maxNumberOfRequiredValues(value: Int): CommandLineOptionStep2[A]        = CommandLineOptionStep2(name, is_required, longNames,          shortNames,          dependencies,             description, arity, minNumberOfRequiredValues, value,                     parser)
    def describedAs(value: String): CommandLineOptionStep2[A]                   = CommandLineOptionStep2(name, is_required, longNames,          shortNames,          dependencies,             value,       arity, minNumberOfRequiredValues, maxNumberOfRequiredValues, parser)
    def dependsOn(value: String): CommandLineOptionStep2[A]                     = CommandLineOptionStep2(name, is_required, longNames,          shortNames,          value :: dependencies,    description, arity, minNumberOfRequiredValues, maxNumberOfRequiredValues, parser)
    def dependsOn(opt: TypedCommandLineOption[Any]): CommandLineOptionStep2[A]  = CommandLineOptionStep2(name, is_required, longNames,          shortNames,          opt.name :: dependencies, description, arity, minNumberOfRequiredValues, maxNumberOfRequiredValues, parser)
    def parseAs[B](value: OptionParser[B]): TypedCommandLineOption[B]           = new TypedCommandLineOption(name, is_required, if (longNames.isEmpty) List(name) else longNames, shortNames, dependencies, description, arity, minNumberOfRequiredValues, maxNumberOfRequiredValues, Some(value))
  }
}

/**
 * Constrains the resulting type to A.
 *
 * @tparam A Type that an argument will be transformed into.
 */
class TypedCommandLineOption[+A](val name: String, val required: Boolean, val longNames: List[String], val shortNames: List[String], val dependencies: List[String], val description: String, val arity: Int, val minNumberOfRequiredValues: Int, val maxNumberOfRequiredValues: Int, val parser: Option[OptionParser[Any]]) {
  def apply(value: String): Option[A] = parser match {
    case None => None
    case Some(arg_parser) => arg_parser(value).asInstanceOf[Option[A]]
  }

  def isMatchForLongName(name_to_match: String): Boolean = longNames.find(_.equalsIgnoreCase(name_to_match)).isDefined
  def isMatchForShortName(name_to_match: String): Boolean = shortNames.find(_.equalsIgnoreCase(name_to_match)).isDefined
  def isMatchForName(name_to_match: String): Boolean = isMatchForLongName(name_to_match) || isMatchForShortName(name_to_match)

  if (minNumberOfRequiredValues != UNBOUNDED && maxNumberOfRequiredValues != UNBOUNDED && maxNumberOfRequiredValues < minNumberOfRequiredValues)
    throw new IllegalArgumentException("maxNumberOfRequiredValues must be >= minNumberOfRequiredValues")

  if (arity != UNBOUNDED && arity < 1)
    throw new IllegalArgumentException("arity must be >= 1 or specified as unbounded")
}
