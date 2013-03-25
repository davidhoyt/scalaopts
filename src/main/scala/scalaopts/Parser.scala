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

import scala.Iterable

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

    val errors: CommandLineOptionParseErrors = Map()

    //Are there any required options that are not present?
    val all_missing_required =
      for {
        entry <- options
        opt = entry._2._1
        if opt.required && processed_results.contains(opt.name)
      }
        yield opt
    val any_missing_required = !all_missing_required.isEmpty
    val errors_1 = if (any_missing_required) errors.updated(ParserError.MissingRequired, all_missing_required) else errors

    //Find options that are missing dependencies
    val all_missing =
      for {
        entry <- results
        name = entry._1 if options.contains(name)
        opt = options(name)._1
        missing = opt.dependencies.filter(!results.contains(_)).toSeq if !missing.isEmpty
      }
        yield opt -> missing.reverse
    val any_missing_dependencies = !all_missing.isEmpty
    val errors_2 = if (any_missing_dependencies) errors_1.updated(ParserError.MissingDependencies, all_missing.toMap) else errors_1

    //Determine if parsing was overall successful or not.
    val success = !any_missing_required && !any_missing_dependencies

    //Send back the results
    new ParseResults(success, processed_results, errors_2, options)
  }
}

/** Simple wrapper for a map so we can access elements by either a string name or an instance of a typed command line option. */
class CommandLineOptionParseErrorMap[A >: CommandLineOptionMapTypedValue, B](val map: Map[A, B]) extends Map[A, B] {
  def iterator = map.iterator
  def get(key: A) = map.get(key)
  def -(key: A) = new CommandLineOptionParseErrorMap[A, B](map.-(key))
  def +[B1 >: B](kv: (A, B1)) = new CommandLineOptionParseErrorMap[A, B1](map + kv)

  def apply(name: String): Option[B] = {
    val entry = map.find(_._1.asInstanceOf[CommandLineOptionMapTypedValue].name.equalsIgnoreCase(name))
    if (entry.isDefined) {
      Some(entry.get._2)
    } else {
      None
    }
  }

  override def apply(opt: A): B =
    map.get(opt).get
}

/** What callers work with. */
class ParseResults(val success: Boolean, val optionResults: CommandLineOptionParseResults, val errors: CommandLineOptionParseErrors, val options: CommandLineOptionMap) {
  def apply[T](name: String): Option[Seq[T]] = optionResults.get(name) match {
    case None => None
    case Some(value) => Some(value.asInstanceOf[Seq[T]])
  }

  def find[T](name: String): Option[Seq[T]] =
    apply(name)

  def first[T](name: String): Option[T] = find(name) match {
    case Some(value) if !value.isEmpty => Some(value.head)
    case Some(value) => defaultFor[T](name)
    case None => None
    case _ => None
  }

  private def defaultFor[T](name: String): Option[T] = options.get(name) match {
    case None => None
    case Some(original) => original._1.defaultValue.asInstanceOf[Option[T]]
  }

  def single[T](name: String): Option[T] = first[Seq[T]](name)
    .map(_.head)
    .orElse {
      defaultFor[T](name)
    }

  def anyMissingRequired:     Boolean = errors.contains(ParserError.MissingRequired)
  def anyMissingDependencies: Boolean = errors.contains(ParserError.MissingDependencies)

  def missingRequired: Iterable[CommandLineOptionMapTypedValue] =
    if (anyMissingRequired) {
      errors(ParserError.MissingRequired).asInstanceOf[Iterable[CommandLineOptionMapTypedValue]]
    } else {
      Iterable()
    }

  def missingDependencies: CommandLineOptionParseErrorMap[CommandLineOptionMapTypedValue, Seq[String]] =
    if (anyMissingDependencies) {
      new CommandLineOptionParseErrorMap[CommandLineOptionMapTypedValue, Seq[String]](errors(ParserError.MissingDependencies).asInstanceOf[Map[CommandLineOptionMapTypedValue, Seq[String]]] withDefaultValue Seq[String]())
    } else {
      new CommandLineOptionParseErrorMap[CommandLineOptionMapTypedValue, Seq[String]](Map() withDefaultValue Seq[String]())
    }

  def visitMissingRequired(visitor: CommandLineOptionMapTypedValue => Boolean): Unit = {
    val iter = missingRequired.iterator
    while(iter.hasNext && visitor(iter.next())) {
      ;
    }
  }

  def visitMissingDependencies(visitor: ((CommandLineOptionMapTypedValue, Seq[String])) => Boolean): Unit = {
    val iter = missingDependencies.iterator
    while(iter.hasNext && visitor(iter.next())) {
      ;
    }
  }

  override def toString: String = optionResults.mkString("\n")
}
