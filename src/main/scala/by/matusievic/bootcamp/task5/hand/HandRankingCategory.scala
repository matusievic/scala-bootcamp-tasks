package by.matusievic.bootcamp.task5.hand

import by.matusievic.bootcamp.task5.domain.Hand

trait HandRankingCategory {
  def weight: Long = 1_00_00_00_00_00L

  def calculateHandWeight(hand: Hand): Option[Long]

  def calculateTieWeight(ranks: Iterable[Int]): Long = {
    ranks.toSeq.sorted.reverse.reduce[Int]((l, r) => l * 100 + r)
  }
}
