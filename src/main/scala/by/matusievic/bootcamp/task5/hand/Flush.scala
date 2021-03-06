package by.matusievic.bootcamp.task5.hand

import by.matusievic.bootcamp.task5.domain.Hand

object Flush extends HandRankingCategory {
  override val weight: Long = 6 * super.weight

  override def calculateHandWeight(hand: Hand): Option[Long] = {
    Option.when(hand.distinctBy(_.suit).length == 1)(calculateTieWeight(hand.map(_.rank)) + weight)
  }
}
