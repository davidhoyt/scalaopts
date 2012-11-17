package scalaopts

import common.StringUtil._
import annotation.tailrec

class Parser(val configuration: ParserConfiguration, options: Map[String, TypedCommandLineOption[_]]) {

  def parse(values: String*) = parseArguments(values)

  //We don't really want to return a boolean - that's just a placeholder for now
  def parseArguments(values: Seq[String]): Boolean = parse0(values.map(s => s.trim.toSeq), None)

  //We don't really want to return a boolean - that's just a placeholder for now
  @tailrec
  private def parse0(values: Seq[Seq[Char]], current_param: Option[Seq[Char]]): Boolean = values match {
    case Nil => {
      println("DONE")
      true
    }
    case Seq(value, tail @_*) => {
      value match {
        case Seq(first_char, param_name @_*) if first_char == configuration.argumentNameSeparator => {
          println("param name: " + param_name.toString())
          parse0(tail, Some(param_name))
        }
        case _ => {
          parse0(tail, current_param)
        }
      }
    }
  }

  //def process
}
