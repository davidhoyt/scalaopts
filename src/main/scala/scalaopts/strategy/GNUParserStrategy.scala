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

  /**
   * @see [[scalaopts.ParserStrategy.toStandardOptionView()]]
   */
  def toStandardOptionView(args: Stream[String], command_line_options: CommandLineOptionMap): Stream[StandardOption[_]] = {
    val findCommandLineOption = findMatchingCommandLineOption(command_line_options)_
    val findCommandLineOptionByLongName = findMatchingCommandLineOptionByLongName(command_line_options)_
    val findCommandLineOptionByShortName = findMatchingCommandLineOptionByShortName(command_line_options)_

    def isLongCommandLineOption(s: String): Boolean = s.startsWith("--")
    def isShortCommandLineOption(s: String): Boolean = s.startsWith("-")
    def isCommandLineOption(s: String): Boolean = isLongCommandLineOption(s) || isShortCommandLineOption(s)

    @tailrec
    def toStandardOptionView0(args: Stream[String]): Stream[StandardOption[_]] = {
      args match {
        case arg #:: tail if arg.isNonEmpty && isCommandLineOption(arg) => {
          logger.info(_ ++= "Found argument: " ++= arg)

          if (isLongCommandLineOption(arg)) {
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
                  toStandardOptionView0(processCommandLineOptionValues(command_line_option, empty, 1, command_line_option.maxNumberOfRequiredValues, tail))
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

            val name = stripLeadingHyphens(arg)

            logger.info("short opt")
            logger.info(_ ++= "name: " ++= name)

            findCommandLineOptionByShortName(name) match {
              case None => {
                unrecognizedArgument(name)
                Stream()
              }
              case Some(command_line_option) => {
                //It's possible this is a flag value -- we might need to process this here and now!
                if (command_line_option.isFlag) {
                  //This is a flag, but it should still be evaluated.
                  processCommandLineOptionValue(command_line_option, empty)

                  //Continue processing.
                  toStandardOptionView0(tail)
                } else {
                  //Process remaining values for this option.
                  toStandardOptionView0(processCommandLineOptionValues(command_line_option, empty, 0, command_line_option.maxNumberOfRequiredValues, tail))
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

    def processCommandLineOptionValues(mapValue: CommandLineOptionMapValue, currentValue: String, valuesFound: Int, valuesRemaining: Int, args: Stream[String]): Stream[String] = {
      @tailrec
      def processCommandLineOptionValues0(currentValue: String, valuesFound: Int, valuesRemaining: Int, args: Stream[String]): Stream[String] = {
        logger.finer("processing remaining option values")
        args match {
          case arg #:: tail if !isCommandLineOption(arg) && findCommandLineOption(arg).isEmpty => {
            logger.finer(_ ++= "found option value: " ++= arg)

            //Ensure we haven't exceeded the max number of values for this argument.
            if (mapValue.maxNumberOfRequiredValues == UNBOUNDED || valuesRemaining > 0) {
              processCommandLineOptionValue(mapValue, arg)
              processCommandLineOptionValues0(arg, if (mapValue.minNumberOfRequiredValues != UNBOUNDED) min(valuesFound + 1, mapValue.minNumberOfRequiredValues) else UNBOUNDED, if (mapValue.maxNumberOfRequiredValues != UNBOUNDED) max(valuesRemaining - 1, -1) else UNBOUNDED, tail)
            } else {
              if (mapValue.maxNumberOfRequiredValues != UNBOUNDED && mapValue.maxNumberOfRequiredValues > 0) {
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
            if (mapValue.minNumberOfRequiredValues != UNBOUNDED && valuesFound < mapValue.minNumberOfRequiredValues) {
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

      processCommandLineOptionValues0(currentValue, valuesFound, valuesRemaining, args)
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
