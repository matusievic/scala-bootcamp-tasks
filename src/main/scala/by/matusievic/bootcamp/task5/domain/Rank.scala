package by.matusievic.bootcamp.task5.domain

sealed abstract case class Rank(order: Int, sign: Char) {
  override def toString: String = sign.toString
}

object Ace extends Rank(14, 'A')
object King extends Rank(13, 'K')
object Queen extends Rank(12, 'Q')
object Jack extends Rank(11, 'J')
object Ten extends Rank(10, 'T')
object Nine extends Rank(9, '9')
object Eight extends Rank(8, '8')
object Seven extends Rank(7, '7')
object Six extends Rank(6, '6')
object Five extends Rank(5, '5')
object Four extends Rank(4, '4')
object Three extends Rank(3, '3')
object Two extends Rank(2, '2')

object Rank {
  implicit def rankToInt(rank: Rank): Int = rank.order

  private val ranks = Seq(
    Ace,
    King,
    Queen,
    Jack,
    Ten,
    Nine,
    Eight,
    Seven,
    Six,
    Five,
    Four,
    Three,
    Two
  )

  def fromSign(sign: Char): Option[Rank] = ranks.find(_.sign == sign)
}