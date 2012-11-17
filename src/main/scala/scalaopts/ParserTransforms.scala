package scalaopts

object ParserTransforms {
  def createParser[A](configuration: ParserConfiguration, options: Seq[TypedCommandLineOption[A]]): Parser = {
    val map = (
      for {
        opt <- options
      }
      yield opt.name -> opt
    ).toMap
    new Parser(configuration, map)
  }
}