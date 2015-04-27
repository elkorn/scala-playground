
scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
) map(_.withSources.withJavadoc)

scalariformSettings
