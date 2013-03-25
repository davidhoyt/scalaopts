package scalaopts

import scala.volatile
import scala.collection.mutable

/**
 */
case class CommandLineOptionResults(val results: CommandLineOptionResultsMap, val errors: CommandLineOptionParseErrors)

//class CommandLineOptionResults[TKey >: CommandLineOptionResultKey, +TValue >: CommandLineOptionResultValue](
//  private[this] val results: CommandLineOptionResultsMap,
//  @volatile private[this] var invalidOptions: mutable.ListBuffer[String] = new mutable.ListBuffer[String]
//) {
//
//  private[scalaopts] def invalidOption(name: String): Unit = invalidOptions += name
//
//  def apply(key: TKey): TValue = results(key.asInstanceOf[CommandLineOptionResultKey])
//  def getOrElse[T >: TValue](key: TKey, default: => T): TValue = results.get(key.asInstanceOf[CommandLineOptionResultKey]) match {
//    case Some(v) => v
//    case None => default.asInstanceOf[CommandLineOptionResultValue]
//  }
//
//  def updated[T >: TValue](key: TKey, value: T) =
//    new CommandLineOptionResults(results.updated(key.asInstanceOf[CommandLineOptionResultKey], value).asInstanceOf[CommandLineOptionResultsMap], invalidOptions)
//}
