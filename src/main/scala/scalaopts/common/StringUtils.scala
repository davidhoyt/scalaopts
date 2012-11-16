package scalaopts.common

/**
 * Extensions for [[java.lang.String]] classes.
 */
object StringUtils {
  implicit final def extend(s: String) = StringExtensions(s)

  val EMPTY = ""

  /** Returns if the string is either null or empty. */
  def isNullOrEmpty(s: String) = (s == null || s.isEmpty || EMPTY == s)

  /** Returns if the string is not null and not empty. */
  def isNonEmpty(s: String) = !isNullOrEmpty(s)

  /** Returns the string if it is not null and not empty, otherwise the empty string. */
  def checked(s: String) = if (isNullOrEmpty(s)) EMPTY else s

  @inline case class StringExtensions(s: String) {
    /** @see [[scalaopts.common.StringUtils.isNullOrEmpty()]] */
    def isNullOrEmpty = StringUtils.isNullOrEmpty(s)

    /** @see [[scalaopts.common.StringUtils.isNonEmpty()]] */
    def isNonEmpty = StringUtils.isNonEmpty(s)

    /** @see [[scalaopts.common.StringUtils.checked()]] */
    def checked = StringUtils.checked(s)
  }
}




