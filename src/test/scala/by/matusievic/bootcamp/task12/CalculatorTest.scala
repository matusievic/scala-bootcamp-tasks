package by.matusievic.bootcamp.task12

import by.matusievic.bootcamp.task12.Calculator.Operation
import org.scalacheck.Prop.propBoolean
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.Checkers.check
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class CalculatorTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  "calculator should" - {
    "sum numbers" in {
      check { nums: Seq[Int] =>
        val normalizedNums = nums.map(_ % 10).filter(_ >= 0)
        var c = Calculator(0, 0, Some(Operation.Plus))
        normalizedNums.foreach { n =>
          c = c.enter(n).map(_.calculate.plus).getOrElse(Calculator())
        }
        c.memory == normalizedNums.sum
      }
    }

    "subtract numbers" in {
      check { nums: Seq[Int] =>
        val normalizedNums = nums.map(_ % 10).filter(_ >= 0)
        var c = Calculator(0, 0, Some(Operation.Minus))
        normalizedNums.foreach { n =>
          c = c.enter(n).map(_.calculate.minus).getOrElse(Calculator())
        }
        c.memory == normalizedNums.fold(0)(_ - _)
      }
    }

    "multiply numbers" in {
      check { nums: Seq[Int] =>
        val normalizedNums = nums.map(_ % 10).filter(_ >= 0)
        var c = Calculator(0, 0, Some(Operation.Multiply))
        normalizedNums.foreach { n =>
          c = c.enter(n).map(_.calculate.multiply).getOrElse(Calculator())
        }
        c.memory == normalizedNums.fold(0)(_ * _)
      }
    }

    "divide numbers" in {
      check { nums: Seq[Int] =>
        val normalizedNums = nums.map(_ % 10).filter(_ > 0)
        var c = Calculator(0, 0, Some(Operation.Divide))
        normalizedNums.foreach { n =>
          c = c.enter(n).map(_.calculate.divide).getOrElse(Calculator())
        }
        c.memory == normalizedNums.fold(0)(_ / _)
      }
    }

    "fail when input out of range" in {
      check { n: Int =>
        val normalizedN = if (n >= 0 && n <= 9) n + 10 else n
        Calculator(0, 0, None).enter(normalizedN) == Left("digit out of range")
      }
    }
  }
}
