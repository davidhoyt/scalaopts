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

import common.StringUtil._
import common.platform.Terminal
import common.{Arch, OSFamily}
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

/**
  */
@RunWith(classOf[JUnitRunner])
class ParserTest extends FunSuite {

  test("StringUtil.toValidIdentifier") {
    assert("".toValidIdentifier == "_")
    assert(" ".toValidIdentifier == "_")
    assert("  ".toValidIdentifier == "__")
    assert("-".toValidIdentifier == "_")
    assert("$!@#%^&*()-+{}[]:\"<>/\\|,.~`'?".toValidIdentifier == "$____________________________")
    assert("$".toValidIdentifier == "$")
    assert("123".toValidIdentifier == "_123")
    assert("abc".toValidIdentifier == "abc")
    assert("abc-123".toValidIdentifier == "abc_123")
  }

  test("translate") {
/*
//http://stackoverflow.com/questions/1025181/hidden-features-of-scala

//push new scope
command_line_options {
  opt: "v" %% "verbose" %% "description" %% BooleanOpt/FlagOpt
  opt: "p" %% "print" %% "print description" %% BooleanOpt
}
//pop scope
*/

    //p.parseArgs("-a", "-p", "-c")
    //val t = new Parser().translate("a", (t => t))
    //
    //assert(t.isInstanceOf[String])

    //need a glob type that gets everything except valid other command_line_options

    println(OSFamily.systemOSFamily)
    println(Arch.systemArch)
    println(Terminal.queryTerminalDimensions())

    val result_1 = CommandLineOption.named("a")
    val result_2 = CommandLineOption named "a"
    val result_3 = CommandLineOption named "a" shortName "b" shortName "c" describedAs "my description" parseAs DefaultIntegerOption
    val result_4 = CommandLineOption named "custom" parseAs new CustomOptionParser[Int](transform = (s: String) => Option(s.length))
    val parser: CommandLineSpecification =
      CommandLineOptions(
        CommandLineOption named "size"
          longName       "size"
          shortName      "s"
          required       YES
          describedAs    "size description"
          arity          UNBOUNDED
          arguments      (1 to 4)
          flag           NO
          default        50
          parseAs        IntegerOption(defaultValue = 100)
          accumulateWith IntegerList(initialValues = List(1, 2, 3)),

        CommandLineOption named "custom"
          default 0
          parseAs new CustomOptionParser[Int](transform = (s: String) => Some(s.length))
          accumulateWith AsyncInteger(callback = i => println("async: " + i)),

        CommandLineOption named "verbose"
          shortName "v"
          dependsOn "size"
          dependsOn "somethingElse"
          describedAs "verbose description"
          parseAs DefaultFlagOption
      )

    //parser.parse("--a", "-verbose", "-c")
    //Tests:
    // INVALID: --<param name> (missing equals)
    //   VALID: --<param name>= (empty value)
    //   VALID: -<short param name> "-value beginning with dash"
    //   VALID: --<long param name>=-value_beginning_with_dash
    //   VALID: -<short param name> value
    //   VALID: --<long param name>=value
    //   VALID: -<short param name> min values=0, max values=0, arity=1 (effectively a flag option)
    //   unbounded max values
    //   bounded max values
    //   -ooo where o is a flag and the arity is 2 (thus making this an error, but -oo would work)
    //parser.parse("--a", "<my value for a!>", "--a=b", "-verbose", "-c")
    //parser.parse("-a", "<my value for a!>")
    //parser.parse("--a=a_value", "-a", "a2_value", "--a=a3_value") //arity
    val parse_results_1 = parser.parse("--size=123", "sz1_value", "sz2_value", "sz3_value", "--size=456")
    val size_options = parse_results_1[Int]("size")
    //val parse_results_2 = parser.parse("--custom=my_value_here")

    assert(parse_results_1.success)
    println(size_options)
  }

  test("test1") {
    println("1")
    val twoPlusTwo = 2 + 2
    assert(twoPlusTwo == 4)
  }

  test("test2") {
    println("2")
    val twoPlusTwo = 2 + 2
    assert(twoPlusTwo == 4)
  }

}
