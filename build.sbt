name := "type-supervised-tagging-2012emnlp"

version := "0.0.1"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "commons-logging" % "commons-logging" % "1.1.1",
  "log4j" % "log4j" % "1.2.16")

mainClass in (Compile, run) := Some("dhgarrette.typesupervisedtagging.run.Run")
