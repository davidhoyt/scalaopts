package scalaopts

/**
 * Builds up command line parsing options. Meant to function as an internal DSL. Used like:
 *
 * Option named "verbose" alias "v" alias "q" describedAs "description" parseAs BooleanOpt,
 * Option named "print" alias "p" describedAs "print description" parseAs IntegerOpt
 *
 */
object Arguments {
  def Arguments(args: TypedArgument[_]*) = new ArgumentContainer

  class ArgumentContainer()

  /**
   * Commands describe available directives that will be used when actually parsing command line
   * arguments down the line.
   */
  sealed trait Command

  /** The starting point of the DSL from which we will build an argument. */
  case object Argument extends Command {
    def named(name: String) = new WrappedArgument(name)
  }

  /** Intended to be created from a [[scalaopts.Arguments.Command]] starting point. */
  sealed trait WrappedCommand

  /**
   * Used as a builder to describe an option.
   *
   * @param name The full name of the command line argument.
   * @param aliases A list of aliases that this command accepts.
   * @param description A description used in usage and help text.
   * @param parser What is intended to convert the argument into a typed value.
   */
  case class WrappedArgument(name: String, aliases: List[String] = List(), description: String = "", parser: Option[ArgumentParser[Any]] = None) extends WrappedCommand {
    def alias(value: String) = WrappedArgument(name, value :: aliases, description, parser)
    def describedAs(value: String) = WrappedArgument(name, aliases, value, parser)
    def parseAs[A](value: ArgumentParser[A]): TypedArgument[A] = TypedArgument(name, aliases, description, Some(value))
  }

  /**
   * Constrains the resulting type to A.
   *
   * @tparam A Type that an argument will be transformed into.
   */
  case class TypedArgument[A](name: String, aliases: List[String], description: String = "", parser: Option[ArgumentParser[A]]) {
    def apply(value: String): Option[A] = parser match {
      case None => None
      case Some(arg_parser) => arg_parser(value)
    }
  }

  /**
   * Function that accepts a string and changes it to a value of type A.
   *
   * @tparam A Result type of applying the function to the given [[java.lang.String]] argument.
   */
  type FnTransform[A] = String => Option[A]
  private val TRANSFORM_BOOLEAN: FnTransform[Boolean] = s => try { Some(s.toBoolean)        } catch { case _: Throwable => None }
  private val TRANSFORM_CHAR:    FnTransform[Char]    = s => try { s.toCharArray.headOption } catch { case _: Throwable => None }
  private val TRANSFORM_BYTE:    FnTransform[Byte]    = s => try { Some(s.toByte)           } catch { case _: Throwable => None }
  private val TRANSFORM_SHORT:   FnTransform[Short]   = s => try { Some(s.toShort)          } catch { case _: Throwable => None }
  private val TRANSFORM_INT:     FnTransform[Int]     = s => try { Some(s.toInt)            } catch { case _: Throwable => None }
  private val TRANSFORM_LONG:    FnTransform[Long]    = s => try { Some(s.toLong)           } catch { case _: Throwable => None }
  private val TRANSFORM_FLOAT:   FnTransform[Float]   = s => try { Some(s.toFloat)          } catch { case _: Throwable => None }
  private val TRANSFORM_DOUBLE:  FnTransform[Double]  = s => try { Some(s.toDouble)         } catch { case _: Throwable => None }

  /**
   * Holds information that will be used later when arguments are evaluated. Used in a builder fashion and meant to
   * be used in the internal DSL.
   *
   * @tparam A Result type of applying the transform.
   */
  sealed trait ArgumentParser[A] {
    def withDefault(default: A): ArgumentParser[A]
    def withTransform(transform: FnTransform[A]): ArgumentParser[A]
    def apply(value: String): Option[A]
  }

  case class CustomArgumentParser[A](default: Option[A] = None, transform: FnTransform[A]) extends ArgumentParser[A] {
    def withDefault(default_value: A) = CustomArgumentParser(Some(default_value), transform)
    def withTransform(new_transform: FnTransform[A]) = CustomArgumentParser(default, new_transform)
    def apply(value: String) = transform(value)
  }

  case class IntegerOpt(defaultValue: Int = 0) extends CustomArgumentParser[Int](Some(defaultValue), TRANSFORM_INT)
  object DefaultIntegerOpt extends IntegerOpt()

  case class BooleanOpt(defaultValue: Boolean = false) extends CustomArgumentParser[Boolean](Some(defaultValue), TRANSFORM_BOOLEAN)
  object DefaultBooleanOpt extends BooleanOpt()
}




