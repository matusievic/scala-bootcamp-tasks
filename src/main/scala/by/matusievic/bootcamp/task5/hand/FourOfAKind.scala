package by.matusievic.bootcamp.task5.hand

import by.matusievic.bootcamp.task5.domain.Hand

object FourOfAKind extends HandRankingCategory {
  override val weight: Long = 8 * super.weight

  override def calculateHandWeight(hand: Hand): Option[Long] = {
    val rankToCount = hand.groupMapReduce(_.rank.order)(_ => 1)(_ + _)
    Option.when(rankToCount.size == 2) {
      val ranks = rankToCount.collect {
        case (rank, count) if count == 4 => rank + 15
        case (rank, _) => rank
      }
      calculateTieWeight(ranks) + weight
    }
  }
}
