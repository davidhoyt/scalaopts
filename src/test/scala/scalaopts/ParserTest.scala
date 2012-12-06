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
    intercept[IllegalArgumentException] {
      CommandLineOptions(
        CommandLineOption named "a" dependsOn "b",
        CommandLineOption named "b" dependsOn "a"
      ).parse()
    }
//    intercept[IllegalArgumentException] {
//      CommandLineOptions(
//        CommandLineOption named "a" dependsOn "b",
//        CommandLineOption named "b" dependsOn "c",
//        CommandLineOption named "c" dependsOn "<non-existent>"
//      ).parse()
//    }
//    intercept[IllegalArgumentException] {
//      CommandLineOptions(
//        CommandLineOption named "a" dependsOn "b",
//        CommandLineOption named "b" dependsOn "c",
//        CommandLineOption named "c" dependsOn "d",
//        CommandLineOption named "d" dependsOn "a"
//      ).parse()
//    }
//    intercept[IllegalArgumentException] {
//      CommandLineOptions(
//        CommandLineOption named "a" dependsOn "b",
//        CommandLineOption named "b" dependsOn "c",
//        CommandLineOption named "c" dependsOn "d",
//        CommandLineOption named "d" dependsOn "c"
//      ).parse()
//    }
    intercept[IllegalArgumentException] {
      CommandLineOptions(
        CommandLineOption named  "7" dependsOn "11" dependsOn  "8",
        CommandLineOption named  "5" dependsOn "11",
        CommandLineOption named  "3" dependsOn  "8" dependsOn "10",
        CommandLineOption named "11" dependsOn  "2" dependsOn  "9" dependsOn "10",
        CommandLineOption named  "8" dependsOn  "9",
        CommandLineOption named  "2",
        CommandLineOption named  "9" dependsOn  "3",
        CommandLineOption named "10"
      ).parse()
    }
  }

  test("valid dependency graphs") {
    //http://en.wikipedia.org/wiki/Topological_sort
    val g_1 =
      CommandLineOptions(
        CommandLineOption named  "7" dependsOn "11" dependsOn  "8",
        CommandLineOption named  "5" dependsOn "11",
        CommandLineOption named  "3" dependsOn  "8" dependsOn "10",
        CommandLineOption named "11" dependsOn  "2" dependsOn  "9" dependsOn "10",
        CommandLineOption named  "8" dependsOn  "9",
        CommandLineOption named  "2",
        CommandLineOption named  "9",
        CommandLineOption named "10"
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
    //TODO: Check dependency cycles: see ParserTransforms

    println(parse_results_1.optionResults)
    assert(parse_results_1.success)
    println(size_options)
    println(first_size_options)
  }
}
