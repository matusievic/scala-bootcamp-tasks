package by.matusievic.bootcamp.task5.domain

case class Card(suit: Suit, rank: Rank) {
  override def toString: String = rank.toString + suit.toString.toLowerCase.head
}

object Card {
  def apply(stringRepresentation: String): Option[Card] = {
    for {
      rank <- Rank.fromSign(stringRepresentation.charAt(0))
      suit <- Suit.fromSign(stringRepresentation.charAt(1))
      if stringRepresentation.length == 2
    } yield Card(suit, rank)
  }
}
