name := "scala-bootcamp-tasks"

version := "0.1"

scalaVersion := "2.13.4"

val scalaTestVersion = "3.1.0.0-RC2"

libraryDependencies ++= Seq(
  "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full)
