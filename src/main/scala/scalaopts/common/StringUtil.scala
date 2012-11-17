package scalaopts.common

/**
 * Extensions for [[java.lang.String]] classes.
 */
object StringUtil {
  implicit final def extend(s: String) = StringExtensions(s)

  val empty = ""

  /** Returns if the string is either null or empty. */
  def isNullOrEmpty(s: String) = (s == null || s.isEmpty || empty == s)

  /** Returns if the string is not null and not empty. */
  def isNonEmpty(s: String) = !isNullOrEmpty(s)

  /** Returns the string if it is not null and not empty, otherwise the empty string. */
  def checked(s: String) = if (isNullOrEmpty(s)) empty else s

  @inline case class StringExtensions(s: String) {
    /** @see [[scalaopts.common.StringUtil.isNullOrEmpty()]] */
    def isNullOrEmpty = StringUtil.isNullOrEmpty(s)

    /** @see [[scalaopts.common.StringUtil.isNonEmpty()]] */
    def isNonEmpty = StringUtil.isNonEmpty(s)

    /** @see [[scalaopts.common.StringUtil.checked()]] */
    def checked = StringUtil.checked(s)
  }
}




