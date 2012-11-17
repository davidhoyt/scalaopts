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

  /**
   * Used as a builder to describe an option.
   *
   * @param name The full name of the command line argument.
   * @param aliases A list of aliases that this command accepts.
   * @param description A description used in usage and help text.
   * @param parser What is intended to convert the argument into a typed value.
   */
  case class CommandLineOptionStep2[A](name: String, aliases: List[String] = List(), dependencies: List[String] = List(), description: String = "", parser: Option[OptionParser[A]] = None) extends Command {
    def alias(value: String): CommandLineOptionStep2[A]                         = CommandLineOptionStep2(name, value :: aliases, dependencies,             description, parser)
    def describedAs(value: String): CommandLineOptionStep2[A]                   = CommandLineOptionStep2(name, aliases,          dependencies,             value,       parser)
    def dependsOn(value: String): CommandLineOptionStep2[A]                     = CommandLineOptionStep2(name, aliases,          value :: dependencies,    description, parser)
    def dependsOn(opt: TypedCommandLineOption[Any]): CommandLineOptionStep2[A]  = CommandLineOptionStep2(name, aliases,          opt.name :: dependencies, description, parser)
    def parseAs[B](value: OptionParser[B]): TypedCommandLineOption[B] = new TypedCommandLineOption(name, aliases, dependencies, description, Some(value))
  }
}

/**
 * Constrains the resulting type to A.
 *
 * @tparam A Type that an argument will be transformed into.
 */
class TypedCommandLineOption[+A](val name: String, val aliases: List[String], val dependencies: List[String], val description: String = "", val parser: Option[OptionParser[Any]]) {
  def apply(value: String): Option[A] = parser match {
    case None => None
    case Some(arg_parser) => arg_parser(value).asInstanceOf[Option[A]]
  }
}
