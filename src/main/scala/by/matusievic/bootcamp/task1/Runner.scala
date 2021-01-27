package by.matusievic.bootcamp.task1

import by.matusievic.bootcamp.task1.Math._

object Runner {
  def main(args: Array[String]): Unit = {
    val numbers = List((17, 2), (6, 4), (9, 6), (12, 4))
    numbers.foreach { case (a, b) => println(buildGcdMessage(a, b)) }
    numbers.foreach { case (a, b) => println(buildLcmMessage(a, b)) }
  }

  def buildMessage(operation: String, a: Int, b: Int, result: Int) = f"$operation($a%5d, $b%5d) = $result%10d"

  def buildGcdMessage(a: Int, b: Int): String = buildMessage("gcd", a, b, gcd(a, b))

  def buildLcmMessage(a: Int, b: Int): String = buildMessage("lcm", a, b, lcm(a, b))

}
