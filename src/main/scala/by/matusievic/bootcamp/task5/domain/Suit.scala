package by.matusievic.bootcamp.task5.domain

sealed abstract case class Suit(sign: Char) {
  override def toString: String = sign.toString
}

object Club extends Suit('C')
object Diamond extends Suit('D')
object Heart extends Suit('H')
object Spade extends Suit('S')

object Suit {
  private val suits = Seq(Club, Diamond, Heart, Spade)

  def fromSign(sign: Char): Option[Suit] = suits.find(_.sign == sign)
}
