package scalaopts

trait ParserTransforms {
  def createParser(configuration: Configuration, args: Seq[TypedArgument[_]]): Parser = {
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
