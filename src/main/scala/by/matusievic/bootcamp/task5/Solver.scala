package by.matusievic.bootcamp.task5

import by.matusievic.bootcamp.task5.domain.{Card, Hand}
import by.matusievic.bootcamp.task5.game.FiveCardDraw

object Solver {
  def process(line: String): String = {
    val ErrorPrefix = "Error: "

    line.toLowerCase.split("\\s+").toList match {
      case "texas-holdem" :: board :: hands   => ErrorPrefix + "The solution doesn't support Texas Hold'em"
      case "omaha-holdem" :: board :: hands   => ErrorPrefix + "The solution doesn't support Omaha Hold'em"
      case "five-card-draw" :: hands          =>
        val value = fromInput(hands)
        val value1 = FiveCardDraw.evaluate(value)
        toOutput(value1)
      case x :: _                             => ErrorPrefix + "Unrecognized game type"
      case _                                  => ErrorPrefix + "Invalid input"
    }
  }

  def fromInput(hands: Seq[String]): Seq[Hand] = {
    hands.map(handString => Hand(handString.toUpperCase.grouped(2).flatMap(Card(_)).toSeq))
  }

  def toOutput(hands: Seq[Hand]): String = {
    hands.map(_.toString).mkString(" ")
  }
}
