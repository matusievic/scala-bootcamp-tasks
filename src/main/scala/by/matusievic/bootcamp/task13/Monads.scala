package by.matusievic.bootcamp.task13

import cats.Monad

object Monads {

  val optionMonad: Monad[Option] = new Monad[Option] {
    // implement me
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case Some(a) => f(a)
      case _ => None
    }

    // implement me
    override def pure[A](x: A): Option[A] = Some(x)

    // cats-only method, don't implement me
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = ???
  }

  def eitherMonad[T]: Monad[Either[T, *]] = new Monad[Either[T, *]] {
    // implement me
    override def flatMap[A, B](fa: Either[T, A])(f: A => Either[T, B]): Either[T, B] = fa match {
      case Right(a) => f(a)
      case Left(t) => Left(t)
    }

    // implement me
    override def pure[A](x: A): Either[T, A] = Right(x)

    // cats-only method, don't implement me
    override def tailRecM[A, B](a: A)(f: A => Either[T, Either[A, B]]): Either[T, B] = ???
  }

  def functionMonad[T]: Monad[T => *] = new Monad[T => *] {
    // implement me
    override def flatMap[A, B](fa: T => A)(f: A => T => B): T => B = { t: T =>
      f(fa(t))(t)
    }

    // implement me
    override def pure[A](x: A): T => A = _ => x

    // cats-only method, don't implement me
    override def tailRecM[A, B](a: A)(f: A => T => Either[A, B]): T => B = ???
  }
}
