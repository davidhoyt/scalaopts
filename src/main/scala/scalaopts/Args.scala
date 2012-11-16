package scalaopts

/** The starting point of the DSL from which we will build an argument. */
case object Argument extends Command {
  def named(name: String) = new Args.WrappedArgument(name = name)
}

/**
 * Constrains the resulting type to A.
 *
 * @tparam A Type that an argument will be transformed into.
 */
case class TypedArgument[A](name: String, aliases: List[String], dependencies: List[String], description: String = "", parser: Option[ArgumentParser[A]]) {
  def apply(value: String): Option[A] = parser match {
    case None => None
    case Some(arg_parser) => arg_parser(value)
  }
}

/**
 * Builds up command line parsing options. Meant to function as an internal DSL. Used like:
 *
 * Option named "verbose" alias "v" alias "q" describedAs "description" parseAs BooleanOpt,
 * Option named "print" alias "p" describedAs "print description" parseAs IntegerOpt
 *
 */
object Args {
  /** Intended to be created from a [[scalaopts.Command]] starting point. */
  sealed trait WrappedCommand

  /**
   * Used as a builder to describe an option.
   *
   * @param name The full name of the command line argument.
   * @param aliases A list of aliases that this command accepts.
   * @param description A description used in usage and help text.
   * @param parser What is intended to convert the argument into a typed value.
   */
  case class WrappedArgument(name: String, aliases: List[String] = List(), dependencies: List[String] = List(), description: String = "", parser: Option[ArgumentParser[Any]] = None) extends WrappedCommand {
    def alias(value: String) = WrappedArgument(name, value :: aliases, dependencies, description, parser)
    def describedAs(value: String) = WrappedArgument(name, aliases, dependencies, value, parser)
    def dependsOn(value: String) = WrappedArgument(name, aliases, value :: dependencies, description, parser)
    def parseAs[A](value: ArgumentParser[A]): TypedArgument[A] = TypedArgument(name, aliases, dependencies, description, Some(value))
  }
}


