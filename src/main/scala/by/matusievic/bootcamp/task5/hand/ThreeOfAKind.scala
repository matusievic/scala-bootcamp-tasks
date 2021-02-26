package by.matusievic.bootcamp.task5.hand

import by.matusievic.bootcamp.task5.domain.Hand

object ThreeOfAKind extends HandRankingCategory {
  override val weight: Long = 4 * super.weight

  override def calculateHandWeight(hand: Hand): Option[Long] = {
    val rankToCount = hand.groupMapReduce(_.rank.order)(_ => 1)(_ + _)
    Option.when(rankToCount.size == 3 && rankToCount.exists { case (_, count) => count == 3 }) {
      val ranks = rankToCount.collect {
        case (rank, count) if count == 3 => rank + 15
        case (rank, _) => rank
      }
      calculateTieWeight(ranks) + weight
    }
  }
}