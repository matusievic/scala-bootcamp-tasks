package by.matusievic.bootcamp.task5.hand

import by.matusievic.bootcamp.task5.domain.Hand

object StraightFlush extends HandRankingCategory {
  override val weight: Long = 9 * super.weight

  override def calculateHandWeight(hand: Hand): Option[Long] = {
    val straightWeight = Straight.calculateHandWeight(hand)
    Option.when(hand.distinctBy(_.suit).length == 1 && straightWeight.isDefined) {
      straightWeight.get - Straight.weight + weight
    }
  }
}
