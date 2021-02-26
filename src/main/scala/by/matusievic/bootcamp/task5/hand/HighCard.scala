package by.matusievic.bootcamp.task5.hand

import by.matusievic.bootcamp.task5.domain.Hand

object HighCard extends HandRankingCategory {
  override val weight: Long = 1 * super.weight

  override def calculateHandWeight(hand: Hand): Option[Long] = {
    Option(super.calculateTieWeight(hand.map(_.rank)) + weight)
  }
}
