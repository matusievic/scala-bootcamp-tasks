package by.matusievic.bootcamp.task5.hand

import by.matusievic.bootcamp.task5.domain.Hand

object TwoPairs extends HandRankingCategory {
  override val weight: Long = 3 * super.weight

  override def calculateHandWeight(hand: Hand): Option[Long] = {
    val rankToCount = hand.groupMapReduce(_.rank.order)(_ => 1)(_ + _)
    Option.when(rankToCount.size == 3) {
      val ranks = rankToCount.collect {
        case (rank, count) if count == 2 => rank + 15
        case (rank, _) => rank
      }
      calculateTieWeight(ranks) + weight
    }
  }
}
