package by.matusievic.bootcamp.task5

import scala.io.StdIn

import Solver.process

object Main {
  def main(args: Array[String]): Unit = Iterator.continually(Option(StdIn.readLine()))
    .takeWhile(_.nonEmpty)
    .foreach { x =>
      x map process foreach println
    }
}
