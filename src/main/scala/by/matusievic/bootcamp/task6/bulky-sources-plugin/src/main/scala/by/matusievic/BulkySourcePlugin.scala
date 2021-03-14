package by.matusievic

import sbt.Keys.sources
import sbt.{Def, _}

object BulkySourcePlugin extends sbt.AutoPlugin {

  object autoImport {
    lazy val bulkySources = TaskKey[Seq[(Int, File)]]("list file with more than <threshold> lines")
    lazy val bulkyThresholdInLines = settingKey[Int]("file threshold")
  }

  import autoImport._

  override def globalSettings: Seq[Def.Setting[_]] = {
    bulkyThresholdInLines := 100
  }

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    bulkySources := Def.task {
      findFiles((Compile / sources).value, bulkyThresholdInLines.value)
    }.value,
    (Test / bulkySources) := Def.task {
      findFiles((Test / sources).value, bulkyThresholdInLines.value)
    }.value
  )

  private def findFiles(files: Seq[File], threshold: Int) = {
    files
      .map(f => sbt.IO.readLines(f).size -> f)
      .filter { case (count, _) => count > threshold }
      .sortBy { case (count, _) => count }
  }

  override def trigger: PluginTrigger = allRequirements
}
