name := "scala-bootcamp-tasks"

version := "0.1"

scalaVersion := "2.13.4"

val catsVersion = "2.3.1"
val catsEffectVersion = "2.4.1"
val catsTaglessVersion = "0.11"
val http4sVersion = "0.21.22"
val http4sJdkVersion = "0.3.6"
val circeVersion = "0.13.0"
val scalajVersion = "2.4.2"
val scalaTestVersion = "3.1.0.0-RC2"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
  "org.http4s" %% "http4s-core" % http4sVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.http4s" %% "http4s-jdk-http-client" % http4sJdkVersion,
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-optics" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.scalaj" %% "scalaj-http" % scalajVersion,
  "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test
)
