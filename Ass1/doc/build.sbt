name := "Ass1"

version := "0.1"

scalaVersion := "2.12.5"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"
//libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
//libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "4.0.2" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")