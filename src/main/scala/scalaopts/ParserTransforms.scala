package scalaopts

trait ParserTransforms {
  def createParser(configuration: ParserConfiguration, args: Seq[TypedCommandLineOption[_]]): Parser = {
    //    val map = (
    //      for {
    //        a <- args
    //      }
    //      yield a.name -> a
    //      ).toMap
    //new Parser(configuration, map)
    null
  }
}
