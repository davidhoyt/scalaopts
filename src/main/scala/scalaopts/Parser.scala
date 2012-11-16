package scalaopts

import StringUtils._
import annotation.tailrec

/**
 */
class ParserConfiguration(val argNameSeparator: Char)
object DEFAULT_PARSER_CONFIGURATION extends ParserConfiguration(
  argNameSeparator = '-'
)

class Parser(config: ParserConfiguration = DEFAULT_PARSER_CONFIGURATION) {
  type FnTranslate = String => Option[Any]
  val TRANSLATOR_BOOLEAN: FnTranslate = s => try { Some(s.toBoolean)        } catch { case _ => None }
  val TRANSLATOR_CHAR:    FnTranslate = s => try { s.toCharArray.headOption } catch { case _ => None }
  val TRANSLATOR_BYTE:    FnTranslate = s => try { Some(s.toByte)           } catch { case _ => None }
  val TRANSLATOR_SHORT:   FnTranslate = s => try { Some(s.toShort)          } catch { case _ => None }
  val TRANSLATOR_INTEGER: FnTranslate = s => try { Some(s.toInt)            } catch { case _ => None }
  val TRANSLATOR_LONG:    FnTranslate = s => try { Some(s.toDouble)         } catch { case _ => None }
  val TRANSLATOR_FLOAT:   FnTranslate = s => try { Some(s.toFloat)          } catch { case _ => None }
  val TRANSLATOR_DOUBLE:  FnTranslate = s => try { Some(s.toDouble)         } catch { case _ => None }

  val DEFAULT_TRANSLATORS = Array(
      TRANSLATOR_BOOLEAN
    , TRANSLATOR_CHAR
    , TRANSLATOR_BYTE
    , TRANSLATOR_SHORT
    , TRANSLATOR_INTEGER
    , TRANSLATOR_LONG
    , TRANSLATOR_FLOAT
    , TRANSLATOR_DOUBLE
  )

/*
  States:
   1. Empty

   2. Named
*/

  def translate[A](value: String, t: String => A): A = t(value)

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
        if (config.argNameSeparator == first_char) {
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
