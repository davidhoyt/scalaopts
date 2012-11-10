import sbt._
import Keys._

/**
 * See README.md for high-level overview
 *
 * Libraries Doc Links
 *
 * SBT
 *  - https://github.com/harrah/xsbt/wiki/Getting-Started-Full-Def
 *  - https://github.com/harrah/xsbt/wiki/Getting-Started-Custom-Settings
 *  - https://github.com/harrah/xsbt/wiki/Getting-Started-More-About-Settings
 *  - https://github.com/harrah/xsbt/wiki/Input-Tasks
 *  - https://github.com/harrah/xsbt/wiki/Tasks
 *  - http://harrah.github.com/xsbt/latest/api/index.html
 *  - https://groups.google.com/forum/?fromgroups#!forum/simple-build-tool
 *
 * Scalatest
 *  - http://doc.scalatest.org/1.8/index.html#org.scalatest.package
 */
object ProjectBuild extends Build {

  /***********************************************************
   * MAIN PROJECT DEFINITION
   */

  lazy val project = Project(id = "project", base = file(".")) settings(
    // put all libs in the lib_managed directory, that way we can distribute eclipse project files
    retrieveManaged := true,
    styleCheckSetting
  )

  val styleCheck = TaskKey[Unit]("checkStyle")

  /**
   * depend on compile to make sure the sources pass the compiler
   */
  val styleCheckSetting = styleCheck <<= (compile in Compile, sources in Compile, streams) map { (_, sourceFiles, s) =>
    val logger = s.log
    val (feedback, score) = StyleChecker.assess(sourceFiles)
    logger.info(feedback)
    logger.info("Style Score: "+ score +" out of "+ StyleChecker.maxResult)
  }

  val checkStyleSetting = styleCheckSetting
}