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

package scalaopts.strategy

import scalaopts._
import scalaopts.common.StringUtil._
import scala.math._
import annotation.tailrec
import com.dongxiguo.fastring.Fastring.Implicits._

/**
 * GNU has laid out a set of rules for creating options and non-options, and
 * they are as follows:
 *
 * - Arguments are options if they begin with a hyphen delimiter (‘-’).
 * - Multiple options may follow a hyphen delimiter in a single token if the
 *   options do not take arguments. Thus, ‘-abc’ is equivalent to ‘-a -b -c’.
 * - Option names are single alphanumeric characters.
 * - Certain options require an argument. For example, the ‘-o’ command of the
 *   ld command requires an argument—an output file name.
 * - An option and its argument may or may not appear as separate tokens. (In
 *   other words, the whitespace separating them is optional.) Thus, ‘-o foo’
 *   and ‘-ofoo’ are equivalent.
 * - Options typically precede other non-option arguments.
 * - The argument ‘--’ terminates all options; any following arguments are
 *   treated as non-option arguments, even if they begin with a hyphen.
 * - A token consisting of a single hyphen character is interpreted as an
 *   ordinary non-option argument.
 * - Options may be supplied in any order, or appear multiple times. The
 *   interpretation is left up to the particular application program.
 *
 * GNU adds long options to these conventions. Long options consist of ‘--’
 * followed by a name made of alphanumeric characters and dashes. Option names
 * are typically one to three words long, with hyphens to separate words.
 * Users can abbreviate the option names as long as the abbreviations are
 * unique.
 *
 * To specify an argument for a long option, write ‘--name=value’. This syntax
 * enables a long option to accept an argument that is itself optional.
 */
class GNUParserStrategy extends ParserStrategy {
  //Configure the logger.
  private[GNUParserStrategy] implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger

  val SHORT_OPTION_PREFIX = "-"
  val LONG_OPTION_PREFIX  = "--"
  val NON_OPTION_ARGUMENT = "-"
  val TERMINATOR          = "--"

  def isTerminator(s: String): Boolean = TERMINATOR.equals(s)
  def isNonOptionArgument(s: String): Boolean = NON_OPTION_ARGUMENT.equals(s)
  def isLongCommandLineOption(s: String): Boolean = s.startsWith(LONG_OPTION_PREFIX)
  def isShortCommandLineOption(s: String): Boolean = s.startsWith(SHORT_OPTION_PREFIX)
  def isCommandLineOption(s: String): Boolean = isLongCommandLineOption(s) || isShortCommandLineOption(s)

  def validateOptions(options: CommandLineOptionMap): Boolean = {
    //Validate that all short names are of length 1 and each name is alphanumeric.
    val invalid_short_name_option = options.find(_._2._1.shortNames.exists(name => name.length != 1 || !Character.isLetterOrDigit(name.charAt(0))))
    if (invalid_short_name_option.isDefined) {
      val option = invalid_short_name_option.get._2._1
      throw new IllegalArgumentException("All short names must be 1 character in length and alpha-numeric for GNU-style parsing. The following option violated this rule: (name: " + option.name + ", short names: [" + (option.shortNames mkString ", ") + "])")
    }

    //Validate that all long names are composed of alpha-numeric characters or dashes.
    val invalid_long_name_option = options.find(_._2._1.longNames.exists(name => name.exists(c => !Character.isLetterOrDigit(c) && !('-' == c))))
    if (invalid_long_name_option.isDefined) {
      val option = invalid_long_name_option.get._2._1
      throw new IllegalArgumentException("All long names must be composed of alpha-numeric characters or hyphens for GNU-style parsing. The following option violated this rule: (name: " + option.name + ", long names: [" + (option.longNames mkString ", ") + "])")
    }

    true
  }

  /**
   * @see [[scalaopts.ParserStrategy]]
   */
  def processOptions(application_arguments: Stream[String], command_line_options: CommandLineOptionMap): CommandLineOptionResults = {
    type OptionArgumentsProcessingResult = (Stream[String], CommandLineOptionMap, CommandLineOptionResults)
    type SingleOptionArgumentProcessingResult = (CommandLineOptionMap, _)

    val findCommandLineOption = findMatchingCommandLineOption(command_line_options)_
    val findCommandLineOptionByLongName = findMatchingCommandLineOptionByLongName(command_line_options)_
    val findCommandLineOptionByShortName = findMatchingCommandLineOptionByShortName(command_line_options)_

    @tailrec
    def processOptions0(application_arguments: Stream[String], command_line_options: CommandLineOptionMap, results: CommandLineOptionResults): CommandLineOptionResults = {
      application_arguments match {
        case potential_option #:: tail if potential_option.isNonEmpty && isCommandLineOption(potential_option) => {
          logger.info(fast"Examining $potential_option")

          if (isTerminator(potential_option)) {

            //No more option parsing if we hit a "--", everything from here on out should be considered
            //a non-option argument.
            //
            //We should do something more intelligent with this -- provide a stream for non-option arguments?
            logger.info("Found terminator")
            results

          } else if (isNonOptionArgument(potential_option)) {

            //Treat as a non-option argument. IOW, there's no value for this guy -- just let the
            //app process it.
            //
            //We should do something more intelligent with this -- provide a stream for non-option arguments?
            logger.info("Found non-option argument")
            results

          } else if (isLongCommandLineOption(potential_option)) {

            //Begin parsing long command line options. These are prefixed with a "--"
            //and may optionally have an equals with an option argument following it.
            val opt = stripLeadingHyphens(potential_option)

            //Divide up the option into the name and value if it contains an equals sign.
            val (name, value, equals_found) = splitAtEquals(opt)

            logger.info(fast"Found long option (name: $name, value: $value)")

            //Attempt to lookup the option and hopefully it exists.
            findCommandLineOptionByLongName(name) match {
              //Unable to find the option.
              case None => {
                unrecognizedOption(name)
                results
              }
              //Found the option.
              case Some((command_line_option, accumulated_values)) => {
                if (!hasReachedMaximumArity(command_line_option, results)) {
                  //If there's an equals sign then process this value and any remaining required values
                  if (equals_found) {

                    //We found at least one option argument, so evaluate it.
                    val (revised_option_map, revised_accumulation) = processSingleOptionArgument(command_line_options, command_line_option, value, accumulated_values)

                    //Evaluate any other remaining arguments.
                    val (revised_tail, revised_option_map_2, revised_results) = processOptionArguments(revised_option_map, command_line_option, 1, command_line_option.maxNumberOfArguments - 1, revised_accumulation, results, tail)
                    processOptions0(revised_tail, revised_option_map_2, revised_results)
                  } else if (command_line_option.isFlag) {
                    //This is a flag, but it should still be evaluated.
                    val (revised_option_map, revised_accumulation) = processSingleOptionArgument(command_line_options, command_line_option, empty, accumulated_values)
                    val revised_results = processOptionArgumentsDone(command_line_option, revised_accumulation, results)

                    //Continue processing.
                    processOptions0(tail, revised_option_map, revised_results)
                  } else {
                    invalidFormat(name, "Missing equals sign for option")
                    results
                  }
                } else {
                  exceededMaximumArity(name, command_line_option.arity)
                  results
                }
              }
            }

          } else if (isShortCommandLineOption(potential_option)) {

            //Get the list of 1+ options
            //Recall that an option like "-abc" is actually equivalent to "-a -b -c" (assuming they're all flags)
            val potentially_multiple_options = stripLeadingHyphens(potential_option)

            logger.info(fast"Found short option(s): $potentially_multiple_options")

            //Check if the first character is an option and *NOT* a flag. If so, treat the rest as a value for that option.
            //Otherwise, pick it off, prepend to the arg stream a hyphen and the rest of the current arg and continue processing.
            potentially_multiple_options.headOption match {
              case None => {
                //Nothing to see here...move along...
                //Apparently there's nothing left to examine.
                processOptions0(tail, command_line_options, results)
              }
              case Some(char_name) => {
                //Convert the character to a string.
                val name = char_name.toString

                //Do we have a short name option by this name?
                findCommandLineOptionByShortName(name) match {
                  case None => {
                    //I don't know who you're talking about so error out of here.
                    unrecognizedOption(name)
                    results
                  }
                  case Some((command_line_option, accumulated_values)) => {
                    //Found an option by that name. Excellent.
                    //Let's see if you're a flag or not. If you're not, then the remaining
                    //text is a value.
                    logger.fine(fast"Recognized option (name: ${command_line_option.name})")

                    if (!hasReachedMaximumArity(command_line_option, results)) {

                      val remaining = potentially_multiple_options.tail

                      if (!command_line_option.isFlag) {
                          val (revised_tail, revised_option_map, revised_results) = processOptionArguments(command_line_options, command_line_option, 0, command_line_option.maxNumberOfArguments, command_line_option.accumulator.initialValue, results, remaining #:: tail)
                          processOptions0(revised_tail, revised_option_map, revised_results)
                      } else {
                        //This is a flag, but it should still be evaluated.
                        val (revised_option_map, revised_accumulation) = processSingleOptionArgument(command_line_options, command_line_option, empty, accumulated_values)

                        //Cycle around again, fooling the code into thinking that we're looking at another
                        //short name. This could result in some interesting scenarios. e.g.:
                        //-ooo: Is that the same flag 3 times? Or is it -o with a value of "oo"?
                        //-abc where a is a flag and b is not: Should c be a value for b then?
                        if (remaining.isNonEmpty) {
                          processOptions0((SHORT_OPTION_PREFIX + remaining) #:: tail, revised_option_map, results)
                        } else {
                          val (revised_tail, revised_option_map_2, revised_results) = processOptionArguments(revised_option_map, command_line_option, 0, command_line_option.maxNumberOfArguments, revised_accumulation, results, tail)
                          processOptions0(revised_tail, revised_option_map_2, revised_results)
                        }
                      }
                    } else {
                      exceededMaximumArity(command_line_option.name, command_line_option.arity)
                      results
                    }
                  }
                }
              }
            }

          } else {

            processOptions0(tail, command_line_options, results)

          }
        }
        case potential_option #:: tail => {
          unrecognizedOption(potential_option)
          processOptions0(tail, command_line_options, results)
        }
        case _ => {
          results
        }
      }
    }

    def processOptionArguments(map: CommandLineOptionMap, mapValue: CommandLineOptionMapTypedValue, valuesFound: Int, valuesRemaining: Int, accumulatedValues: Any, results: CommandLineOptionResults, args: Stream[String]): OptionArgumentsProcessingResult = {

      @tailrec
      def processOptionArguments0(valuesFound: Int, valuesRemaining: Int, revised: SingleOptionArgumentProcessingResult, results: CommandLineOptionResults, args: Stream[String]): OptionArgumentsProcessingResult = {
        logger.finer("processing remaining option arguments")

        val accumulatedValues = revised._2

        args match {
          case arg #:: tail if !isCommandLineOption(arg) && findCommandLineOption(arg).isEmpty => {
            logger.finer(fast"found option argument: $arg")

            //Ensure we haven't exceeded the max number of arguments for this option.
            if (mapValue.isMaxNumberOfArgumentsUnbounded || valuesRemaining > 0) {
              val revised_by_single = processSingleOptionArgument(map, mapValue, arg, accumulatedValues)
              processOptionArguments0(if (!mapValue.isMinNumberOfArgumentsUnbounded) min(valuesFound + 1, mapValue.minNumberOfArguments) else UNBOUNDED, if (!mapValue.isMaxNumberOfArgumentsUnbounded) max(valuesRemaining - 1, -1) else UNBOUNDED, revised_by_single, results, tail)
            } else {
              if (!mapValue.isMaxNumberOfArgumentsUnbounded && mapValue.maxNumberOfArguments > 0) {
                exceededMaximumNumberOfArguments(mapValue.name, mapValue.maxNumberOfArguments)
              }

              //Notify accumulators that we're done
              //Not sure if this is correct. If there are more arguments that need processing, then
              //this may prematurely cut it off.
              val revised_results = processOptionArgumentsDone(mapValue, accumulatedValues, results)

              //return unmodified stream at this point so the caller
              //can continue inspecting the arguments at the point where
              //we've left off.
              //
              //IOW, we're explicitly NOT returning tail!
              (args, updatedCommandLineOptionMap(map, mapValue, accumulatedValues), revised_results)
            }
          }
          case _ => {
            logger.finer("no more option arguments, continuing on")

            //Validate that we've met the minimum number of required arguments for this option.
            if (!mapValue.isMinNumberOfArgumentsUnbounded && valuesFound < mapValue.minNumberOfArguments) {
              missingMinimumNumberOfArguments(mapValue.name, valuesFound, mapValue.minNumberOfArguments)
            }

            //Notify accumulators that we're done
            val revised_results = processOptionArgumentsDone(mapValue, accumulatedValues, results)

            //return unmodified stream at this point so the caller
            //can continue inspecting the arguments at the point where
            //we've left off.
            //
            //IOW, we're explicitly NOT returning tail!
            (args, updatedCommandLineOptionMap(map, mapValue, accumulatedValues), revised_results)
          }
        }
      }

      processOptionArguments0(valuesFound, valuesRemaining, (command_line_options, accumulatedValues), results, args)
    }

    def processSingleOptionArgument(map: CommandLineOptionMap, mapValue: CommandLineOptionMapTypedValue, currentValue: String, accumulatedValues: Any): SingleOptionArgumentProcessingResult = {
      logger.info(fast"processing value for ${mapValue.name}: $currentValue")
      val result = mapValue(currentValue)
      logger.fine(fast"ran option parser for ${mapValue.name}, result: ${result.toString}")
      logger.fine(fast"processing accumulator for ${mapValue.name}")
      val accumulation = if (result.isDefined) mapValue.accumulator(result.get, accumulatedValues) else accumulatedValues
      logger.fine(fast"completed processing accumulator for ${mapValue.name}")
      (updatedCommandLineOptionMap(map, mapValue, accumulation), accumulation)
    }

    def processOptionArgumentsDone(mapValue: CommandLineOptionMapTypedValue, accumulatedValues: Any, results: CommandLineOptionResults): CommandLineOptionResults = {
      logger.info(fast"completed processing arguments for ${mapValue.name}")

      val accumulator_result = mapValue.accumulator.done(accumulatedValues)
      val result_option_list = results.getOrElse(mapValue.name, Some(List())).getOrElse(List())
      results.updated(mapValue.name, Some(accumulator_result :: result_option_list))
    }

    def unrecognizedOption(optionName: String): Unit =
      logger.warning(fast"unrecognized option: $optionName")

    def invalidFormat(optionName: String, description: String): Unit =
      logger.warning(fast"invalid format for option. $description")

    def missingMinimumNumberOfArguments(optionName: String, number_found: Int, minimum: Int): Unit =
      logger.warning(fast"missing minimum number of expected arguments for $optionName: ${minimum.toString }, found: ${number_found.toString}")

    def exceededMaximumNumberOfArguments(optionName: String, maximum: Int): Unit =
      logger.warning(fast"exceeded maximum number of expected option arguments for $optionName: ${maximum.toString}")

    def exceededMaximumArity(optionName: String, maximum: Int): Unit =
      logger.warning(fast"exceeded the maximum number of expected options for $optionName: ${maximum.toString}")

    def updatedCommandLineOptionMap(map: CommandLineOptionMap, mapValue: CommandLineOptionMapTypedValue, accumulation: Any): CommandLineOptionMap =
      map.updated(mapValue.name, (mapValue, accumulation))

    def hasReachedMaximumArity(mapValue: CommandLineOptionMapTypedValue, results: CommandLineOptionResults): Boolean = {
      if (!mapValue.isArityUnbounded) {
        results.get(mapValue.name) match {
          case Some(Some(l)) => l.length >= mapValue.arity
          case _ => false
        }
      } else {
        false
      }
    }

    val end_results = processOptions0(application_arguments, command_line_options, Map())
    val reversed_results = end_results.mapValues(opts => Some(opts.getOrElse(List.empty).reverse))
    reversed_results
  }
}
