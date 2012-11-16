package scalaopts

import common.StringUtils._
import annotation.tailrec

class Parser(configuration: Configuration, arguments: Map[String, TypedArgument[_]]) {

  def parseArgs(values: String*) = parse(values.toSeq)

  //We don't really want to return a boolean - that's just a placeholder for now
  def parse(values: Seq[String]): Boolean = parse(values, None)

  //We don't really want to return a boolean - that's just a placeholder for now
  @tailrec
  private def parse(values: Seq[String], current_param: Option[String]): Boolean = values match {
    case Nil => println("DONE"); true
    case Seq(value, tail @_*) =>
      if (value.isNonEmpty && value.headOption.isDefined) {
        val first_char = value.head
        if (configuration.argumentNameSeparator == first_char) {
          //We have a named parameter
          //Find associated option
          val param_name = value.tail
          println("param_name: " + param_name)
          parse(tail, Some(param_name))
        } else {
          parse(tail, current_param)
        }
      } else {
        parse(tail, current_param)
      }
  }

  //def process
}
