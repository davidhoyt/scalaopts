
/** Provides classes for command line argument parsing.
  * Also provides implicits for [[java.lang.String]] processing.
  *
  * ==Overview==
  * Begin by defining options. For example:
  * {{{
  *   options {
  *     opt: "v" %% "verbose" %% "description" %% BooleanOpt/FlagOpt
  *     opt: "p" %% "print" %% "print description" %% BooleanOpt
  *   }
  *   option named "verbose" alias "v" alias "q" describedAs "description" parseAs Boolean,
  *   option named "print" alias "p" describedAs "print description" parseAs Boolean
  * }}}
 */
package object scalaopts {
  object CommandLineOptions {
    object DEFAULT_PARSER_CONFIGURATION extends ParserConfiguration(
      argumentNameSeparator = '-'
    )

    def apply[A](args: TypedCommandLineOption[A]*): Parser = apply(DEFAULT_PARSER_CONFIGURATION)(args: _*)
    def apply[A](configuration: ParserConfiguration)(args: TypedCommandLineOption[A]*): Parser = {
      ParserTransforms.createParser(configuration, args)
    }
  }
}
