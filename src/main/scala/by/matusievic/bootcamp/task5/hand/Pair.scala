package by.matusievic.bootcamp.task5.hand

import by.matusievic.bootcamp.task5.domain.Hand

object Pair extends HandRankingCategory {
  override val weight: Long = 2 * super.weight

  override def calculateHandWeight(hand: Hand): Option[Long] = {
    Option.when(hand.distinctBy(_.rank).length == 4) {
      val ranks = hand.groupMapReduce(_.rank.order)(_ => 1)(_ + _).collect {
        case (rank, count) if count == 2 => rank + 15
        case (rank, _) => rank
      }
      calculateTieWeight(ranks) + weight
    }
  }
}
