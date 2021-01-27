package by.matusievic.bootcamp.task1

import scala.annotation.tailrec

object Math {
  @tailrec
  def gcd(a: Int, b: Int): Int = a match {
    case 0 => b
    case _ => gcd(b % a, a)
  }

  def lcm(a: Int, b: Int): Int = (a * b) / gcd(a, b)
}
