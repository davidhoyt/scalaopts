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
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._
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

  test("dependencies with cycles") {
    //Cycles in dependencies are fine as long as every option in the cycle is defined.

    //They'll parse alright, but if both aren't defined, then expect an error.
    val p_1 =
      CommandLineOptions(
        CommandLineFlag named "a" dependsOn "b",
        CommandLineFlag named "b" dependsOn "a"
      )
      .parse("--a")
    p_1.anyMissingDependencies should be (true)

    //There's a cycle, but both are defined so there's no problem.
    val p_2 =
      CommandLineOptions(
        CommandLineFlag named "a" dependsOn "b",
        CommandLineFlag named "b" dependsOn "a"
      )
      .parse("--a", "--b")
    p_2.anyMissingDependencies should be (false)

    val p_3 =
      CommandLineOptions(
        CommandLineFlag named "a" dependsOn "b",
        CommandLineFlag named "b" dependsOn "c",
        CommandLineFlag named "c" dependsOn "<non-existent>"
      )
      .parse("--a", "--b")
    p_3.anyMissingDependencies should be (true)
    p_3.missingDependencies("b").get should be (List("c"))

    val p_4 =
      CommandLineOptions(
        CommandLineFlag named "a" dependsOn "b",
        CommandLineFlag named "b" dependsOn "c",
        CommandLineFlag named "c" dependsOn "d",
        CommandLineFlag named "d" dependsOn "a"
      )
      .parse("--a")
    p_4.anyMissingDependencies should be (true)
    p_4.missingDependencies("a") should be (Some(List("b")))
    p_4.missingDependencies("b") should be (None) //?
    p_4.missingDependencies("c") should be (None) //?

    val p_5 =
      CommandLineOptions(
        CommandLineFlag named "a" dependsOn "b",
        CommandLineFlag named "b" dependsOn "c",
        CommandLineFlag named "c" dependsOn "d",
        CommandLineFlag named "d" dependsOn "c"
      )
      .parse("--a", "--b", "--c", "--d")
    p_5.anyMissingDependencies should be (false)

    //More advanced cycle: 3 -> 8 -> 9 -> 3
    val opts_6 =
      CommandLineOptions(
        CommandLineFlag named  "7" dependsOn "11" dependsOn  "8",
        CommandLineFlag named  "5" dependsOn "11",
        CommandLineFlag named  "3" dependsOn  "8" dependsOn "10",
        CommandLineFlag named "11" dependsOn  "2" dependsOn  "9" dependsOn "10",
        CommandLineFlag named  "8" dependsOn  "9",
        CommandLineFlag named  "2",
        CommandLineFlag named  "9" dependsOn  "3",
        CommandLineFlag named "10"
      )
    val p_6 = opts_6 parse ("--11", "--2")
    p_6.anyMissingDependencies should be (true)
    p_6.missingDependencies("11").get should be (List("9", "10"))
  }

  test("valid dependency graphs") {
    //http://en.wikipedia.org/wiki/Topological_sort
    val g_1 =
      CommandLineOptions(
        CommandLineFlag named  "7" dependsOn "11" dependsOn  "8",
        CommandLineFlag named  "5" dependsOn "11",
        CommandLineFlag named  "3" dependsOn  "8" dependsOn "10",
        CommandLineFlag named "11" dependsOn  "2" dependsOn  "9" dependsOn "10",
        CommandLineFlag named  "8" dependsOn  "9",
        CommandLineFlag named  "2",
        CommandLineFlag named  "9",
        CommandLineFlag named "10"
      )
      .parse()
    assert(g_1.success)
  }

  test("translate") {
    //http://stackoverflow.com/questions/1025181/hidden-features-of-scala

    //need a glob type that gets everything except valid other command_line_options

    val parser: CommandLineSpecification =
      CommandLineOptions(
        CommandLineOption named "size"
          longName       "size"
          shortName      "s"
          required       YES
          describedAs    "size description"
          arity          UNBOUNDED
          arguments      (1 to 1)
          flag           NO
          default        50
          parseAs        IntegerOption(defaultValue = 100)
          accumulateWith IntegerList(),

        CommandLineFlag named "flag",

        CommandLineOption named "custom"
          default 0
          parseAs new CustomOptionParser[Int](transform = (s: String) => Some(s.length))
          accumulateWith AsyncInteger(callback = i => println("async: " + i)),

        CommandLineOption named "verbose"
          shortName "v"
          dependsOn "size"
          dependsOn "somethingElse"
          describedAs "verbose description",

        CommandLineOption named "string1" shortName "z"
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
    //   cyclic dependencies (a depends on b, b depends on a)
    //parser.parse("--a", "<my value for a!>", "--a=b", "-verbose", "-c")
    //parser.parse("-a", "<my value for a!>")
    //parser.parse("--a=a_value", "-a", "a2_value", "--a=a3_value") //arity
    val parse_results_1 = parser.parse("--size=123", "sz1_value", "sz2_value", "sz3_value", "-znn", "-zmm", "--size=456")
    val size_options = parse_results_1[Seq[Int]]("size")
    val first_size_options = parse_results_1.first[Seq[Int]]("size")
    //val parse_results_2 = parser.parse("--custom=my_value_here")

    //TODO: Fix GNU parser to return something that indicates a failed parsing -- would like to avoid throwing exceptions...

    println(parse_results_1.optionResults)
    //assert(parse_results_1.success)
    println(size_options)
    println(first_size_options)
  }
}
