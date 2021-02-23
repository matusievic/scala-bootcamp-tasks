package by.matusievic.bootcamp.task4

import by.matusievic.bootcamp.task4.Collections._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random

class CollectionsSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "findGap" should "find gap" in {
    findGap(List(1, 2, 3, 5, 6)) shouldEqual Some(3, 5)
  }

  "findGap" should "work correctly on empty" in {
    findGap(List.empty) shouldEqual None
  }

  "findGap" should "work correctly on no gaps" in {
    findGap((1 to 100).toList) shouldEqual None
  }

  "min" should "work correctly on empty" in {
    min(Nil) shouldEqual None
  }

  "min" should "work correctly on non empty" in {
    min(Random.shuffle(1 to 100).toList) shouldEqual Some(1)
  }

  "scanLeft" should "work correctly on numbers" in {
    val numbers = (1 to 100).toList
    scanLeft(0)(numbers)(_ + _) shouldEqual numbers.scanLeft(0)(_ + _)
  }

  "scanLeft" should "work correctly on letters" in {
    val letters = ('a' to 'z').toList.map(_.toString)
    scanLeft("")(letters)(_ + _) shouldEqual letters.scanLeft("")(_ + _)
  }

  "count" should "pass" in {
    count("aaaabbbcca") shouldEqual List(('a', 4), ('b', 3), ('c', 2), ('a', 1))
  }


  "runningSum" should "pass" in {
    runningSum(Array(1, 2, 3, 4)) shouldEqual Array(1, 3, 6, 10)
  }

  "runningSum" should "return empty array on empty array" in {
    runningSum(Array.empty) shouldEqual Array.empty
  }

  "runningSum" should "work correctly on single item array" in {
    runningSum(Array(3)) shouldEqual Array(3)
  }


  "shuffle" should "pass" in {
    shuffle(Array(1, 2, 3, 4, 4, 3, 2, 1), 4) shouldEqual Array(1, 4, 2, 3, 3, 2, 4, 1)
  }

  "shuffle" should "work correctly on empty array" in {
    shuffle(Array.empty, 0) shouldEqual Array.empty
  }

  "shuffle" should "return the same array when invalid input" in {
    shuffle(Array(1, 2, 3), 7) shouldEqual Array(1, 2, 3)
  }


  "maximumWealth" should "pass" in {
    maximumWealth(Array(Array(1, 5), Array(7, 3), Array(3, 5))) shouldEqual 10
  }

  "maximumWealth" should "work correctly with equals wealth" in {
    maximumWealth(Array(Array(1, 2, 3), Array(3, 2, 1))) shouldEqual 6
  }

  "maximumWealth" should "return 0 on empty array" in {
    maximumWealth(Array.empty) shouldEqual 0
  }


  "kidsWithCandies" should "pass" in {
    kidsWithCandies(Array(2, 3, 5, 1, 3), 3) shouldEqual Array(true, true, true, false, true)
  }

  "kidsWithCandies" should "work correctly with equals wealth" in {
    kidsWithCandies(Array(2, 3, 5, 1, 3), 3) shouldEqual Array(true, true, true, false, true)
  }

  "kidsWithCandies" should "return empty array on empty array" in {
    kidsWithCandies(Array.empty, 1) shouldEqual Array.empty
  }


  "maxWidthOfVerticalArea" should "pass" in {
    maxWidthOfVerticalArea(Array(Array(3, 1), Array(9, 0), Array(1, 0), Array(1, 4), Array(5, 3), Array(8, 8))) shouldEqual 3
  }

  "maxWidthOfVerticalArea" should "work correctly with broken points when only y is absent" in {
    maxWidthOfVerticalArea(Array(Array(2), Array(3))) shouldEqual 1
  }

  "maxWidthOfVerticalArea" should "return 0 with broken points when both x and y are absent" in {
    maxWidthOfVerticalArea(Array(Array.empty, Array.empty)) shouldEqual 0
  }

  "maxWidthOfVerticalArea" should "return 0 on empty array" in {
    maxWidthOfVerticalArea(Array.empty) shouldEqual 0
  }

}

