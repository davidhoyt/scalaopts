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

import common.{Arch, OSFamily}
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

/**
  */
@RunWith(classOf[JUnitRunner])
class ParserTest extends FunSuite {

  test("translate") {
/*
//http://stackoverflow.com/questions/1025181/hidden-features-of-scala

//push new scope
options {
  opt: "v" %% "verbose" %% "description" %% BooleanOpt/FlagOpt
  opt: "p" %% "print" %% "print description" %% BooleanOpt
}
//pop scope
*/

    //p.parseArgs("-a", "-p", "-c")
    //val t = new Parser().translate("a", (t => t))
    //
    //assert(t.isInstanceOf[String])

    //need a glob type that gets everything except valid other options

    println(OSFamily.systemOSFamily)
    println(Arch.systemArch)

    val result_1 = CommandLineOption.named("a")
    val result_2 = CommandLineOption named "a"
    val result_3 = CommandLineOption named "a" alias "b" alias "c" describedAs "my description" parseAs DefaultIntegerOption
    val result_4 = CommandLineOption named "custom" parseAs new CustomOptionParser[Int](transform = (s: String) => Option(s.length))
    val parser = CommandLineOptions(
      CommandLineOption named "size" alias "s" alias "sz" describedAs "size description" parseAs IntegerOption(defaultValue = 100),
      CommandLineOption named "verbose" alias "v" dependsOn "size" dependsOn "somethingElse" describedAs "verbose description" parseAs DefaultFlagOption,
      CommandLineOption named "custom" parseAs new CustomOptionParser[Int](transform = (s: String) => Option(s.length))
    )
    println(result_3.aliases)
    println(result_3("234").getOrElse(98765))

    parser.parse("-a", "-verbose", "-c")
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
