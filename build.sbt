name := "scalaopts"

version := "0.1"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-deprecation", "-Xelide-below", "900")

javacOptions ++= Seq("-Xlint:unchecked")

libraryDependencies += "org.scala-lang" % "scala-library" % "2.10.1" % "provided"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.10.1"

libraryDependencies += "net.java.dev.jna" % "jna" % "3.5.1"

libraryDependencies += "com.dongxiguo" % "zero-log" % "0.3.3" % "compile"

libraryDependencies += "org.scalatest" % "scalatest" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"
