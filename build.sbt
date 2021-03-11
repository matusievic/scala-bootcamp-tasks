name := "scala-bootcamp-tasks"

version := "0.1"

scalaVersion := "2.13.4"

val catsVersion = "2.2.0"
val scalaTestVersion = "3.1.0.0-RC2"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test
)
