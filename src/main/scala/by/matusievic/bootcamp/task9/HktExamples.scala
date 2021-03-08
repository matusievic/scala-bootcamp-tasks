package by.matusievic.bootcamp.task9


object SemigroupTask extends App {
  // 1. Semigroup
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    def apply[A: Semigroup]: Semigroup[A] = implicitly
  }

  implicit class SemigroupOps[A: Semigroup](val a: A) {
    def combine(b: A): A = Semigroup[A].combine(a, b)
  }

  // 1.2. Implement Semigroup for Long, String
  implicit def semigroupInt: Semigroup[Int] = _ + _

  implicit def semigroupLong: Semigroup[Long] = _ + _

  implicit def semigroupString: Semigroup[String] = _ + _

  // 1.3. Implement combineAll(list: List[A]) for non-empty lists
  def combineAll[A: Semigroup](ls: List[A]): A = {
    ls.reduce(Semigroup[A].combine)
  }

  println {
    combineAll(List(1, 2, 3)) == 6
  }

  // 1.4. Implement combineAll(list: List[A], startingElement: A) for all lists
  def combineAll[A: Semigroup](ls: List[A], startingElement: A): A = ls.foldLeft(startingElement)(_ combine _)

  println {
    combineAll(List(1, 2, 3), 0) == 6
  }
  println {
    combineAll(List(), 1) == 1
  }
}

object MonoidTask extends App {

  import SemigroupTask.Semigroup

  // 2. Monoid
  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A: Monoid]: Monoid[A] = implicitly
  }

  // 2.2. Implement Monoid for Long, String
  implicit val monoidLong: Monoid[Long] = new Monoid[Long] {
    override def empty: Long = 0

    override def combine(x: Long, y: Long): Long = x + y
  }
  implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0

    override def combine(x: Int, y: Int): Int = x + y
  }
  implicit val monoidString: Monoid[String] = new Monoid[String] {
    override def empty: String = ""

    override def combine(x: String, y: String): String = x + y
  }

  // 2.3. Implement combineAll(list: List[A]) for all lists
  def combineAll[A: Monoid](ls: List[A]): A = ls.foldLeft(Monoid[A].empty)(Monoid[A].combine)

  println {
    combineAll(List(1, 2, 3)) == 6
  }

  // 2.4. Implement Monoid for Option[A]
  implicit def monoidOption[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def empty: Option[A] = None

    override def combine(maybeX: Option[A], maybeY: Option[A]): Option[A] = (maybeX, maybeY) match {
      case (Some(x), Some(y)) => Some(Semigroup[A].combine(x, y))
      case (x, y) => x.orElse(y)
    }
  }

  println {
    combineAll(List(Some(1), None, Some(3))) == Some(4)
  }
  println {
    combineAll(List[Option[Int]](None, None)) == None
  }
  println {
    combineAll(List[Option[Int]]()) == None
  }

  // 2.5. Implement Monoid for Function1 (for result of the function)
  implicit def monoidFunction1[A, B: Monoid]: Monoid[A => B] = new Monoid[A => B] {
    override def empty: A => B = _ => Monoid[B].empty

    override def combine(x: A => B, y: A => B): A => B = a => Monoid[B].combine(x(a), y(a))
  }

  println {
    combineAll(List((a: String) => a.length, (a: String) => a.toInt)).apply("1") == ((a: String) => a.length + a.toInt) ("1")
  }
  println {
    combineAll(List((a: String) => a.length, (a: String) => a.toInt)).apply("123") == 126
  }
}

object FunctorTask extends App {

  // 3. Functor
  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {
    def apply[F[_] : Functor]: Functor[F] = implicitly
  }

  implicit class FunctorOps[F[_] : Functor, A](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
  }

  implicit def optionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit def mapFunctor[K]: Functor[Map[K, *]] = new Functor[Map[K, *]] {
    override def fmap[A, B](fa: Map[K, A])(f: A => B): Map[K, B] = fa.map {
      case (k, v) => k -> f(v)
    }
  }
}

object SemigroupalTask extends App {

  import FunctorTask.{Functor, FunctorOps}

  // 4. Semigroupal
  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  object Semigroupal {
    def apply[A[_] : Semigroupal]: Semigroupal[A] = implicitly
  }

  implicit class SemigoupalOps[A, F[_] : Semigroupal](fa: F[A]) {
    def product[B](fb: F[B]): F[(A, B)] = Semigroupal[F].product(fa, fb)
  }

  // 4.2. Implement Semigroupal for Option
  implicit val semigroupalOption: Semigroupal[Option] = new Semigroupal[Option] {
    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = for {
      a <- fa
      b <- fb
    } yield (a, b)
  }

  // 4.3. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]
  implicit class MapNOps[F[_] : Semigroupal : Functor, A, B](t: (F[A], F[B])) {
    def mapN[R](f: (A, B) => R): F[R] = t match {
      case (fa, fb) => fa.product(fb).fmap {
        case (a, b) => f(a, b)
      }
    }
  }
  println {
    (Option(1), Option(2)).mapN(_ + _) == Some(3)
  }
  println {
    (Option(1), None: Option[Int]).mapN(_ + _) == None
  }

  // 4.4. Implement Semigroupal for Map
  implicit def semigroupalMap[K]: Semigroupal[Map[K, *]] = new Semigroupal[Map[K, *]] {
    override def product[A, B](fa: Map[K, A], fb: Map[K, B]): Map[K, (A, B)] = for {
      (ka, va) <- fa
      (kb, vb) <- fb
      if ka == kb
    } yield ka -> (va, vb)
  }

  println {
    (Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _) == Map(2 -> "bc")
  }
}

// 5. Applicative
object ApplicativeTask extends App {

  import FunctorTask.{Functor, FunctorOps}
  import SemigroupalTask.{SemigoupalOps, Semigroupal}

  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](x: A): F[A]
  }

  object Applicative {
    def apply[F[_] : Applicative]: Applicative[F] = implicitly
  }

  // 5.1. Implement Applicative for Option, Either
  implicit val applicativeEither: Applicative[Either[String, *]] = new Applicative[Either[String, *]] {
    override def pure[A](x: A): Either[String, A] = Right(x)

    override def product[A, B](fa: Either[String, A], fb: Either[String, B]): Either[String, (A, B)] = for {
      a <- fa
      b <- fb
    } yield (a, b)

    override def fmap[A, B](fa: Either[String, A])(f: A => B): Either[String, B] = fa.map(f)
  }

  implicit def applicativeOption: Applicative[Option] = new Applicative[Option] {
    override def pure[A](x: A): Option[A] = Some(x)

    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = for {
      a <- fa
      b <- fb
    } yield (a, b)

    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit def applicativeList: Applicative[List] = new Applicative[List] {
    override def pure[A](x: A): List[A] = List(x)

    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa zip fb

    override def fmap[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  def traverse[A, B, F[_] : Applicative](as: List[A])(f: A => F[B]): F[List[B]] = {
    as.foldLeft(Applicative[F].pure[List[B]](List.empty[B])) {
      case (acc: F[List[B]], cur: A) => acc.product(f(cur)).fmap[List[B]] {
        case (ls, i) => ls :+ i
      }
    }
  }

  println {
    traverse(List(1, 2, 3)) { i =>
      Option.when(i % 2 == 1)(i)
    } == None
  }

  println {
    traverse(List(1, 2, 3)) { i =>
      Some(i + 1): Option[Int]
    } == Some(List(2, 3, 4))
  }
}

object FoldableTask extends App {

  import ApplicativeTask.{Applicative, applicativeOption}
  import FunctorTask.FunctorOps
  import SemigroupalTask.SemigoupalOps

  // 6. Foldable
  // 6.1. Implement Foldable with `foldLeft`
  trait Foldable[F[_]] {
    def empty[R]: F[R]

    def foldLeft[T, R](f: F[T])(empty: R)(op: (R, T) => R): R

    def :+[T](f: F[T])(t: T): F[T]
  }

  object Foldable {
    def apply[F[_] : Foldable]: Foldable[F] = implicitly
  }

  implicit class FoldableOps[F[_] : Foldable, T](f: F[T]) {
    def empty[R]: F[R] = Foldable[F].empty[R]

    def foldLeft[R](empty: R)(op: (R, T) => R): R = Foldable[F].foldLeft(f)(empty)(op)

    def :+(t: T): F[T] = Foldable[F].:+(f)(t)
  }

  // 6.2. Implement Foldable for List
  implicit def foldableList: Foldable[List] = new Foldable[List] {
    override def empty[R]: List[R] = List.empty[R]

    override def foldLeft[T, R](f: List[T])(empty: R)(op: (R, T) => R): R = f.foldLeft(empty)(op)

    override def :+[T](f: List[T])(t: T): List[T] = f :+ t
  }

  // 6.3. Implement `traverse` for all Foldables instead of List
  def traverse[X, Y, A[_] : Applicative, F[_] : Foldable](as: F[X])(f: X => A[Y]): A[F[Y]] = {
    as.foldLeft(Applicative[A].pure[F[Y]](Foldable[F].empty[Y])) {
      case (acc: A[F[Y]], cur: X) => acc.product(f(cur)).fmap[F[Y]] {
        case (ls, i) => ls :+ i
      }
    }
  }

  println {
    traverse(List(1, 2, 3)) { i =>
      Option.when(i % 2 == 1)(i)
    } == None
  }
}
