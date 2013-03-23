name := "scalaopts"

version := "0.1"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-deprecation", "-feature", "-Xelide-below", "900")

javacOptions ++= Seq("-Xlint:unchecked")


libraryDependencies += "net.java.dev.jna" % "jna" % "3.5.1"

libraryDependencies += "com.dongxiguo" %% "zero-log" % "0.3.3" % "compile"

libraryDependencies += "com.dongxiguo" %% "fastring" % "0.2.1" % "compile"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.11" % "test"
