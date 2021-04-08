package by.matusievic.bootcamp.task15

import by.matusievic.bootcamp.task15.EffectsHomework1.IO
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.concurrent.ExecutionContext
import scala.util.Try

class EffectsHomework1Spec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "IO" should "be able to map" in {
    forAll { (initial: Int, expected: Int) =>
      IO(initial).map(_ => expected).unsafeRunSync() shouldBe expected
    }
  }

  "IO" should "be able to flatMap" in {
    forAll { (initial: Int, expected: Int) =>
      IO(initial).flatMap(_ => IO(expected)).unsafeRunSync() shouldBe expected
    }
  }

  "IO" should "be able to *>" in {
    forAll { (initial: Int, expected: Int) =>
      (IO(initial) *> IO(expected)).unsafeRunSync() shouldBe expected
    }
  }

  "IO" should "return expected value" in {
    forAll { (initial: Int, expected: Int) =>
      IO(initial).as(expected).unsafeRunSync() shouldBe expected
    }
  }

  "IO" should "return void" in {
    forAll { x: Int =>
      IO(x).void.unsafeRunSync() shouldBe()
    }
  }

  "IO" should "attempt and return Right" in {
    forAll { x: Int =>
      IO(x).attempt.unsafeRunSync() shouldBe Right(x)
    }
  }

  "IO" should "attempt and return Left" in {
    forAll { t: Throwable =>
      IO(throw t).attempt.unsafeRunSync() shouldBe Left(t)
    }
  }

  "IO" should "return Some" in {
    forAll { x: Int =>
      IO(x).option.unsafeRunSync() shouldBe Some(x)
    }
  }

  "IO" should "return None" in {
    IO(null).option.unsafeRunSync() shouldBe None
  }

  "IO" should "handle error with" in {
    forAll { (t: Throwable, expected: Int) =>
      IO(throw t).handleErrorWith(_ => IO(expected)).unsafeRunSync() shouldBe expected
    }
  }

  "IO" should "redeem" in {
    forAll { expected: Int =>
      IO(()).redeem(_ => IO(expected), identity).unsafeRunSync() shouldBe()
    }
  }

  "IO" should "redeem with" in {
    forAll { expected: Int =>
      IO(()).redeemWith(_ => IO(expected), _ => IO(expected)).unsafeRunSync() shouldBe expected
    }
  }

  "IO" should "return result" in {
    forAll { expected: Int =>
      IO(expected).unsafeRunSync() shouldBe expected
    }
  }

  "IO" should "return future" in {
    implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global
    forAll { expected: Int =>
      IO(expected).unsafeToFuture().map(_ shouldBe expected)
    }
  }

  "IO" should "be able to apply" in {
    forAll { expected: Int =>
      IO(expected).unsafeRunSync() shouldBe expected
    }
  }

  "IO" should "be able to suspend" in {
    forAll { expected: Int =>
      IO.suspend(IO(expected)).unsafeRunSync() shouldBe expected
    }
  }

  "IO" should "be able to delay" in {
    forAll { expected: Int =>
      IO.delay(expected).unsafeRunSync() shouldBe expected
    }
  }

  "IO" should "be able to pure" in {
    forAll { expected: Int =>
      IO.pure(expected).unsafeRunSync() shouldBe expected
    }
  }

  "IO" should "be able to construct from either" in {
    forAll { expected: Int =>
      IO.fromEither(Right(expected)).unsafeRunSync() shouldBe expected
    }
  }

  "IO" should "be able to throw from either" in {
    forAll { expected: Throwable =>
      assertThrows[Throwable] {
        IO.fromEither(Left(expected): Either[Throwable, Int]).unsafeRunSync()
      }
    }
  }

  "IO" should "be able to construct from some" in {
    forAll { (t: Throwable, expected: Int) =>
      IO.fromOption(Some(expected))(t).unsafeRunSync() shouldBe expected
    }
  }

  "IO" should "be able to throw from none" in {
    forAll { (t: Throwable, expected: Int) =>
      assertThrows[Throwable] {
        IO.fromOption(None: Option[Int])(t).unsafeRunSync()
      }
    }
  }

  "IO" should "be able to construct from success" in {
    forAll { expected: Int =>
      IO.fromTry(Try(expected)).unsafeRunSync() shouldBe expected
    }
  }

  "IO" should "be able to throw from failure" in {
    forAll { t: Throwable =>
      assertThrows[Throwable] {
        IO.fromTry(Try[Int](throw t)).unsafeRunSync()
      }
    }
  }

  "IO" should "be able to construct from none" in {
    IO.none.unsafeRunSync() shouldBe None
  }

  "IO" should "be able to raise error" in {
    forAll { t: Throwable =>
      assertThrows[Throwable] {
        IO.raiseError[Int](t).unsafeRunSync() shouldBe t
      }
    }
  }

  "IO" should "be able not to raise if unless condition fail" in {
    forAll { t: Throwable =>
      IO.raiseUnless(cond = true)(t).unsafeRunSync() shouldBe()
    }
  }

  "IO" should "be able to raise if unless condition succeed" in {
    forAll { t: Throwable =>
      assertThrows[Throwable] {
        IO.raiseUnless(cond = false)(t).unsafeRunSync()
      }
    }
  }

  "IO" should "be able not to raise if when condition fail" in {
    forAll { t: Throwable =>
      IO.raiseWhen(cond = false)(t).unsafeRunSync() shouldBe()
    }
  }

  "IO" should "be able to raise if when condition succeed" in {
    forAll { t: Throwable =>
      assertThrows[Throwable] {
        IO.raiseWhen(cond = true)(t).unsafeRunSync()
      }
    }
  }

  "IO" should "be able to return IO if unless condition fail" in {
    IO.unlessA(cond = false)(IO.unit).unsafeRunSync() shouldBe ()
  }

  "IO" should "be able to return unit if unless condition succeed" in {
    forAll { any: Int =>
      IO.unlessA(cond = true)(IO(any)).unsafeRunSync() shouldBe()
    }
  }

  "IO" should "be able to return IO if when condition succeed" in {
    IO.whenA(cond = true)(IO.unit).unsafeRunSync() shouldBe()
  }

  "IO" should "be able to return unit if when condition fail" in {
    forAll { any: Int =>
      IO.whenA(cond = false)(IO(any)).unsafeRunSync() shouldBe()
    }
  }

  "IO" should "be able to construct unit" in {
    IO.unit.unsafeRunSync() shouldBe()
  }

}
