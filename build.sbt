name := "scalaopts"

version := "0.1"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-deprecation", "-Xelide-below", "900")

javacOptions ++= Seq("-Xlint:unchecked")

libraryDependencies += "net.java.dev.jna" % "jna" % "3.5.1"

libraryDependencies += "com.dongxiguo" %% "zero-log" % "0.1.2" % "compile"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"
