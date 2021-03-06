package by.matusievic.bootcamp.task5.game

import by.matusievic.bootcamp.task5.domain.Hand
import by.matusievic.bootcamp.task5.hand._

object FiveCardDraw {
  def evaluate(hands: Seq[Hand]): Seq[Hand] = {
    hands.filter(_.length == 5).sortBy(evaluateHand)
  }

  private def evaluateHand(hand: Hand): Long = {
    rankingCategories.flatMap(_.calculateHandWeight(hand)).head
  }

  private val rankingCategories: Seq[HandRankingCategory] = Seq(
    StraightFlush,
    FourOfAKind,
    FullHouse,
    Flush,
    Straight,
    ThreeOfAKind,
    TwoPairs,
    Pair,
    HighCard
  )
}
