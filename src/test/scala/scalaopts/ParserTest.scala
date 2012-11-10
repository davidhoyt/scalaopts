package scalaopts

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

/**
  */
@RunWith(classOf[JUnitRunner])
class ParserTest extends FunSuite {

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
