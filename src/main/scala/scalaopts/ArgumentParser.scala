package scalaopts

import common.StringUtils
import Transforms._

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

/**
 * Defines a custom parser that transforms a [[java.lang.String]] into a value.
 *
 * @param default The default value to use if the argument is not defined.
 * @param requiresAssociatedValue True if, when processed, the next value parsed should be applied to this instance.
 * @param transform Function that transforms the [[java.lang.String]] into a value.
 * @tparam A Result type of applying the transform.
 */
case class CustomArgumentParser[A](default: Option[A] = None, requiresAssociatedValue: Boolean = true, transform: FnTransform[A]) extends ArgumentParser[A] {
  def withDefault(default_value: A) = CustomArgumentParser(Some(default_value), requiresAssociatedValue, transform)
  def withTransform(new_transform: FnTransform[A]) = CustomArgumentParser(default, requiresAssociatedValue, new_transform)
  def withRequiresAssociatedValue(value: Boolean) = CustomArgumentParser(default, value, transform)
  def apply(value: String) = transform(value)
}

case class ByteOpt(defaultValue: Byte = 0) extends CustomArgumentParser[Byte](Some(defaultValue), true, TRANSFORM_BYTE)
object DefaultByteOpt extends ByteOpt()

case class ShortOpt(defaultValue: Short = 0) extends CustomArgumentParser[Short](Some(defaultValue), true, TRANSFORM_SHORT)
object DefaultShortOpt extends ShortOpt()

case class IntegerOpt(defaultValue: Int = 0) extends CustomArgumentParser[Int](Some(defaultValue), true, TRANSFORM_INT)
object DefaultIntegerOpt extends IntegerOpt()

case class LongOpt(defaultValue: Long = 0L) extends CustomArgumentParser[Long](Some(defaultValue), true, TRANSFORM_LONG)
object DefaultLongOpt extends LongOpt()

case class FloatOpt(defaultValue: Float = 0.0f) extends CustomArgumentParser[Float](Some(defaultValue), true, TRANSFORM_FLOAT)
object DefaultFloatOpt extends FloatOpt()

case class DoubleOpt(defaultValue: Double = 0.0D) extends CustomArgumentParser[Double](Some(defaultValue), true, TRANSFORM_DOUBLE)
object DefaultDoubleOpt extends DoubleOpt()

case class BooleanOpt(defaultValue: Boolean = false) extends CustomArgumentParser[Boolean](Some(defaultValue), true, TRANSFORM_BOOLEAN)
object DefaultBooleanOpt extends BooleanOpt()

case class FlagOpt() extends CustomArgumentParser[Boolean](Some(true), false, TRANSFORM_NOOP)
object DefaultFlagOpt extends FlagOpt()

case class CharOpt(defaultValue: Char = '\0') extends CustomArgumentParser[Char](Some(defaultValue), true, TRANSFORM_CHAR)
object DefaultCharOpt extends CharOpt()

case class StringOpt(defaultValue: String = StringUtils.EMPTY) extends CustomArgumentParser[String](Some(defaultValue), true, TRANSFORM_STRING)
object DefaultStringOpt extends StringOpt()
