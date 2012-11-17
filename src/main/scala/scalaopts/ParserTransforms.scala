package scalaopts

object ParserTransforms {
  def createParser(configuration: ParserConfiguration, options: Seq[TypedCommandLineOption[_]]): Parser = {
    val map = (
      for {
        opt <- options
      }
      yield opt.name -> opt
    ).toMap
    new Parser(configuration, map)
  }
}