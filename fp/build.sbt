
scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.3" % "test"
) map(_.withSources.withJavadoc)

scalariformSettings
