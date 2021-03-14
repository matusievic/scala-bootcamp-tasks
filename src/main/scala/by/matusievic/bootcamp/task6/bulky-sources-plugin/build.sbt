
ThisBuild / version := "0.1.0"
ThisBuild / organization := "by.matusievic.bootcamp"

lazy val root = (project in file("."))
  .enablePlugins(SbtPlugin, BulkySourcePlugin)
  .settings(
    name := "bulky-sources-plugin",
    pluginCrossBuild / sbtVersion := {
      scalaBinaryVersion.value match {
        case "2.12" => "1.2.8" // set minimum sbt version
      }
    }
  )