package scalaopts

/** The starting point of the DSL from which we will build an argument. */
case object CommandLineOption extends Command {
  def named[A](name: String) = CommandLineOptionStep2[A](name = name)

  /**
   * Used as a builder to describe an option.
   *
   * @param name The full name of the command line argument.
   * @param aliases A list of aliases that this command accepts.
   * @param description A description used in usage and help text.
   * @param parser What is intended to convert the argument into a typed value.
   */
  case class CommandLineOptionStep2[A](name: String, aliases: List[String] = List(), dependencies: List[String] = List(), description: String = "", parser: Option[OptionParser[A]] = None) extends Command {
    def alias(value: String)                         = CommandLineOptionStep2(name, value :: aliases, dependencies,             description, parser)
    def describedAs(value: String)                   = CommandLineOptionStep2(name, aliases,          dependencies,             value,       parser)
    def dependsOn(value: String)                     = CommandLineOptionStep2(name, aliases,          value :: dependencies,    description, parser)
    def dependsOn(opt: TypedCommandLineOption[Any])  = CommandLineOptionStep2(name, aliases,          opt.name :: dependencies, description, parser)
    def parseAs[B](value: OptionParser[B]): TypedCommandLineOption[B] = TypedCommandLineOption(name, aliases, dependencies, description, Some(value))
  }
}

/**
 * Constrains the resulting type to A.
 *
 * @tparam A Type that an argument will be transformed into.
 */
case class TypedCommandLineOption[+A](name: String, aliases: List[String], dependencies: List[String], description: String = "", parser: Option[OptionParser[Any]]) {
  def apply(value: String): Option[A] = parser match {
    case None => None
    case Some(arg_parser) => arg_parser(value).asInstanceOf[Option[A]]
  }
}
