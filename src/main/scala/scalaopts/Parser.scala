package scalaopts

/**
 */
class Parser {
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

  def translate[A](value: String, t: String => A): A = t(value)
}
