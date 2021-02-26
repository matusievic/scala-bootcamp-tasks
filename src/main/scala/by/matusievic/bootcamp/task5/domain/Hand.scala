package by.matusievic.bootcamp.task5.domain

case class Hand(cards: Seq[Card]) extends AnyVal {
  override def toString: String = cards.mkString("")
}

object Hand {
  implicit def HandToSeq(hand: Hand): Seq[Card] = hand.cards
}
