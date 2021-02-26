package by.matusievic.bootcamp.task5.hand

import by.matusievic.bootcamp.task5.domain._

object Straight extends HandRankingCategory {
  override val weight: Long = 5 * super.weight

  override def calculateHandWeight(hand: Hand): Option[Long] = {
    val sortedRanks = hand.map(_.rank.order).sorted
    val rankValues = if (sortedRanks.contains(Two.order) && sortedRanks.count(_ == Ace.order) == 1) {
      sortedRanks.map(r => if (r == Ace.order) Two - 1 else r)
    } else sortedRanks
    Option.when(sortedRanks.distinct.size == sortedRanks.size && areConsecutive(rankValues))(calculateTieWeight(rankValues) + weight)
  }

  private def areConsecutive(ranks: Seq[Int]): Boolean = ranks.sliding(2).count(n => n(0) + 1 == n(1)) == (ranks.length - 1)

}
