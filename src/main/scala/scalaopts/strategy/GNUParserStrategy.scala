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
import util.logging.Logged

class GNUParserStrategy extends ParserStrategy {
  /* Configure the logger. */
  private[GNUParserStrategy] object Log {
    val (logger, formatter) = ZeroLoggerFactory.newLogger(this)
  }

  import Log.logger
  import Log.formatter._

  val SHORT_OPTION_PREFIX = "-"
  val LONG_OPTION_PREFIX = "--"
  val NON_OPTION_ARGUMENT = "-"
  val TERMINATOR = "--"

  def validateOptions(options: CommandLineOptionMap): Boolean = {
    //Validate that all short names are of length 1 and each name is alphanumeric.
    val invalid_short_name_option = options.find(_._2.shortNames.exists(name => name.length != 1 || !Character.isLetterOrDigit(name.charAt(0))))
    if (invalid_short_name_option.isDefined) {
      val option = invalid_short_name_option.get._2
      throw new IllegalArgumentException("All short names must be 1 character in length and alpha-numeric for GNU-style parsing. The following option violated this rule: (name: " + option.name + ", short names: [" + (option.shortNames mkString ", ") + "])")
    }

    //Validate that all long names are composed of alpha-numeric characters or dashes.
    val invalid_long_name_option = options.find(_._2.longNames.exists(name => name.exists(c => !Character.isLetterOrDigit(c) && !('-' == c))))
    if (invalid_long_name_option.isDefined) {
      val option = invalid_long_name_option.get._2
      throw new IllegalArgumentException("All long names must be composed of alpha-numeric characters or hyphens for GNU-style parsing. The following option violated this rule: (name: " + option.name + ", long names: [" + (option.longNames mkString ", ") + "])")
    }

    true
  }

  /**
   * @see [[scalaopts.ParserStrategy.toStandardOptionView()]]
   */
  def toStandardOptionView(args: Stream[String], command_line_options: CommandLineOptionMap): Stream[StandardOption[_]] = {
    val findCommandLineOption = findMatchingCommandLineOption(command_line_options)_
    val findCommandLineOptionByLongName = findMatchingCommandLineOptionByLongName(command_line_options)_
    val findCommandLineOptionByShortName = findMatchingCommandLineOptionByShortName(command_line_options)_

    def isTerminator(s: String): Boolean = TERMINATOR.equals(s)
    def isNonOptionArgument(s: String): Boolean = NON_OPTION_ARGUMENT.equals(s)
    def isLongCommandLineOption(s: String): Boolean = s.startsWith(LONG_OPTION_PREFIX)
    def isShortCommandLineOption(s: String): Boolean = s.startsWith(SHORT_OPTION_PREFIX)
    def isCommandLineOption(s: String): Boolean = isLongCommandLineOption(s) || isShortCommandLineOption(s)

    @tailrec
    def toStandardOptionView0(args: Stream[String]): Stream[StandardOption[_]] = {
      args match {
        case arg #:: tail if arg.isNonEmpty && isCommandLineOption(arg) => {
          logger.info(_ ++= "Found argument: " ++= arg)

          if (isTerminator(arg)) {
            //No more option parsing if we hit a "--", everything from here on out should be considered
            //a non-option argument.
            //
            //We should do something more intelligent with this -- provide a stream for non-option arguments?
            Stream()
          } else if (isNonOptionArgument(arg)) {
            //Treat as a non-option argument. IOW, there's no value for this guy -- just let the
            //app process it.
            //
            //We should do something more intelligent with this -- provide a stream for non-option arguments?
            Stream()
          } else if (isLongCommandLineOption(arg)) {
            val opt = stripLeadingHyphens(arg)
            val (name, value, equals_found) = splitAtEquals(opt)

            logger.info("long opt")
            logger.info(_ ++= "arg name: " ++= name ++= ", value: " ++= value)

            findCommandLineOptionByLongName(name) match {
              case None => {
                unrecognizedArgument(name)
                Stream()
              }
              case Some(command_line_option) => {
                //If there's an equals sign then process this value and any remaining required values
                if (equals_found) {

                  //We found at least one value, so evaluate it.
                  processCommandLineOptionValue(command_line_option, value)

                  //Evaluate any other remaining values.
                  toStandardOptionView0(processCommandLineOptionValues(command_line_option, 1, command_line_option.maxNumberOfRequiredValues - 1, tail))
                } else if (command_line_option.isFlag) {
                  //This is a flag, but it should still be evaluated.
                  processCommandLineOptionValue(command_line_option, empty)

                  //Continue processing.
                  toStandardOptionView0(tail)
                } else {
                  invalidFormat(name, "Missing equals sign for option")
                  Stream()
                }
              }
            }
          } else if (isShortCommandLineOption(arg)) {

            //Get the list of 1+ options
            //Recall that an option like "-abc" is actually equivalent to "-a -b -c" (assuming they're all flags)
            val potentially_multiple_options = stripLeadingHyphens(arg)

            logger.info("short opt")
            logger.info(_ ++= "option(s): " ++= potentially_multiple_options)

            //DOH -- An option and its argument may or may not appear as separate tokens. (In other words, the whitespace separating them is optional.) Thus, ‘-o foo’ and ‘-ofoo’ are equivalent.
            //Check if the first character is an option and *NOT* a flag. If so, treat the rest as a value for that option.
            //Otherwise, pick it off, pre-pend to the arg stream a hyphen and the rest of the current arg and continue processing...
            potentially_multiple_options.headOption match {
              case None => {
                //Nothing to see here...move along...
                //Apparently there's nothing left to examine.
                toStandardOptionView0(tail)
              }
              case Some(char_name) => {
                //Convert the character to a string.
                val name = char_name.toString
                val remaining = potentially_multiple_options.tail

                //Do we have a short name option by this name?
                findCommandLineOptionByShortName(name) match {
                  case None => {
                    //I don't know who you're talking about so error out of here.
                    unrecognizedArgument(name)
                    Stream()
                  }
                  case Some(command_line_option) => {
                    //Found an argument by that name. Excellent.
                    //Let's see if you're a flag or not. If you're not, then the remaining
                    //text is a value.
                    if (!command_line_option.isFlag) {
                      toStandardOptionView0(processCommandLineOptionValues(command_line_option, 0, command_line_option.maxNumberOfRequiredValues, remaining #:: tail))
                    } else {
                      //This is a flag, but it should still be evaluated.
                      processCommandLineOptionValue(command_line_option, empty)

                      //Cycle around again, fooling the code into thinking that we're looking at another
                      //short name. This could result in some interesting scenarios. e.g.:
                      //-ooo: Is that the same flag 3 times? Or is it -o with a value of "oo"?
                      //-abc where a is a flag and b is not: Should c be a value for b then?
                      if (remaining.isNonEmpty) {
                        toStandardOptionView0((SHORT_OPTION_PREFIX + remaining) #:: tail)
                      } else {
                        toStandardOptionView0(processCommandLineOptionValues(command_line_option, 0, command_line_option.maxNumberOfRequiredValues, tail))
                      }
                    }
                  }
                }
              }
            }
          } else {
            toStandardOptionView0(tail)
          }
        }
        case arg #:: tail => {
          logger.warning(_ ++= "Failed to recognize argument: " ++= arg)
          toStandardOptionView0(tail)
        }
        case _ => Stream()
      }
    }

    def processCommandLineOptionValues(mapValue: CommandLineOptionMapValue, valuesFound: Int, valuesRemaining: Int, args: Stream[String]): Stream[String] = {
      @tailrec
      def processCommandLineOptionValues0(valuesFound: Int, valuesRemaining: Int, args: Stream[String]): Stream[String] = {
        logger.finer("processing remaining option values")
        args match {
          case arg #:: tail if !isCommandLineOption(arg) && findCommandLineOption(arg).isEmpty => {
            logger.finer(_ ++= "found option value: " ++= arg)

            //Ensure we haven't exceeded the max number of values for this argument.
            if (mapValue.isMaxNumberOfRequiredValuesUnbounded || valuesRemaining > 0) {
              processCommandLineOptionValue(mapValue, arg)
              processCommandLineOptionValues0(if (!mapValue.isMinNumberOfRequiredValuesUnbounded) min(valuesFound + 1, mapValue.minNumberOfRequiredValues) else UNBOUNDED, if (!mapValue.isMaxNumberOfRequiredValuesUnbounded) max(valuesRemaining - 1, -1) else UNBOUNDED, tail)
            } else {
              if (!mapValue.isMaxNumberOfRequiredValuesUnbounded && mapValue.maxNumberOfRequiredValues > 0) {
                exceededMaximumNumberOfValues(mapValue.name, mapValue.maxNumberOfRequiredValues)
              }

              //return unmodified stream at this point so the caller
              //can continue inspecting the arguments at the point where
              //we've left off.
              //
              //IOW, we're explicitly NOT returning tail!
              args
            }
          }
          case _ => {
            logger.finer("no more option values, continuing on")

            //Validate that we've met the minimum number of required values for this argument.
            if (!mapValue.isMinNumberOfRequiredValuesUnbounded && valuesFound < mapValue.minNumberOfRequiredValues) {
              missingMinimumNumberOfValues(mapValue.name, valuesFound, mapValue.minNumberOfRequiredValues)
            }

            //return unmodified stream at this point so the caller
            //can continue inspecting the arguments at the point where
            //we've left off.
            //
            //IOW, we're explicitly NOT returning tail!
            args
          }
        }
      }

      processCommandLineOptionValues0(valuesFound, valuesRemaining, args)
    }

    def processCommandLineOptionValue(mapValue: CommandLineOptionMapValue, currentValue: String): Unit = {
      logger.info(_ ++= "processing value for " ++= mapValue.name ++= ": " ++= currentValue)
    }

    def unrecognizedArgument(argumentName: String): Unit = {
      logger.warning(_ ++= "unrecognized argument: " ++= argumentName)
    }

    def invalidFormat(argumentName: String, description: String): Unit = {
      logger.warning(_ ++= "invalid format for argument. " + description)
    }

    def missingMinimumNumberOfValues(argumentName: String, number_found: Int, minimum: Int): Unit = {
      logger.warning(_ ++= "missing minimum number of expected values: " ++= minimum.toString ++= ", found: " ++= number_found.toString)
    }

    def exceededMaximumNumberOfValues(argumentName: String, maximum: Int): Unit = {
      logger.warning(_ ++= "exceeded maximum number of expected values: " ++= maximum.toString)
    }

    toStandardOptionView0(args)
  }
}
