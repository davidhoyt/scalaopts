package scalaopts

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

    val result_1 = scalaopts.Argument.named("a")
    val result_2 = scalaopts.Argument named "a"
    val result_3 = scalaopts.Argument named "a" alias "b" alias "c" describedAs "my description" parseAs DefaultIntegerOpt
    val args = Arguments(
      Argument named "size" alias "s" alias "sz" describedAs "size description" parseAs IntegerOpt(defaultValue = 100),
      Argument named "verbose" alias "v" dependsOn "size" dependsOn "somethingElse" describedAs "verbose description" parseAs DefaultFlagOpt
    )
    println(result_3.aliases)
    println(result_3("234").getOrElse(98765))
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
