package scalaopts

class Configuration(val argumentNameSeparator: Char)

object DEFAULT_CONFIGURATION extends Configuration(
  argumentNameSeparator = '-'
)
