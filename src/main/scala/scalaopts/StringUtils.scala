package scalaopts

/**
 * Extensions for [[java.lang.String]] classes.
 */
object StringUtils {
  implicit final def extend(s: String) = StringExtensions(s)

  val EMPTY = ""

  @inline case class StringExtensions(s: String) {
    /** Returns if the string is either null or empty. */
    def isNullOrEmpty = (s == null || s.isEmpty || EMPTY == s)

    /** Returns if the string is not null and not empty. */
    def isNonEmpty = !isNullOrEmpty

    /** Returns the string if it is not null and not empty, otherwise the empty string. */
    def checked = if (isNullOrEmpty) EMPTY else s
  }
}




