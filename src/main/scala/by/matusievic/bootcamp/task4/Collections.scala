package by.matusievic.bootcamp.task4

import scala.annotation.tailrec


object Collections extends App {

  /*
     In a sorted list find two numbers which have a gap between
        None for List(1, 2, 3, 4)
        Some((2, 8)) for List(1, 2, 8)
   */
  def findGap(l: List[Int]): Option[(Int, Int)] = l match {
    case Nil => None
    case _ :: tail => l.zip(tail).find { case (l, r) => math.abs(l - r) != 1 }
  }

  // min list value
  def min(list: List[Int]): Option[Int] = {
    list.foldLeft[Option[Int]](None)((res, cur) => res.filter(_ < cur).orElse(Some(cur)))
  }

  // Implement scanLeft (not using scans ofc)
  def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] = {
    list.foldLeft(List(zero))((acc, cur) => f(acc.head, cur) :: acc).reverse
  }

  // https://twitter.com/allenholub/status/1357115515672555520/photo/1
  def count(s: String): List[(Char, Int)] = {
    @tailrec
    def countIter(acc: List[(Char, Int)], remaining: List[Char]): List[(Char, Int)] = (acc, remaining) match {
      case (_, Nil) => acc.reverse
      case (Nil, r :: rs) => countIter(List((r, 1)), rs)
      case ((a, count) :: as, r :: rs) if a != r => countIter((r, 1) :: acc, rs)
      case ((a, count) :: as, r :: rs) if a == r => countIter((r, count + 1) :: as, rs)
    }

    countIter(Nil, s.toList)
  }

  // https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(nums: Array[Int]): Array[Int] = {
    scanLeft(0)(nums.toList)(_ + _).tail.toArray
  }

  //https://leetcode.com/problems/shuffle-the-array/
  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    if (nums.length == 2 * n) {
      nums.indices.take(n).flatMap(i => Array(nums(i), nums(n + i))).toArray
    } else {
      nums
    }
  }

  //https://leetcode.com/problems/richest-customer-wealth/
  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(_.sum).maxOption.getOrElse(0)
  }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    candies.maxOption.map(max => candies.map(_ + extraCandies.abs >= max)).getOrElse(Array.empty)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points/
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    points.flatMap(_.headOption).sorted match {
      case xs if xs.length <= 1 => 0
      case xs => xs.zip(xs.tail).map { case (l, r) => r - l }.max
    }
  }

}
