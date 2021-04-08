package by.matusievic.bootcamp.task15

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object EffectsHomework1 {
  final class IO[A](run: () => A) {
    def map[B](f: A => B): IO[B] = IO(f(run()))

    def flatMap[B](f: A => IO[B]): IO[B] = f(run())

    def *>[B](another: IO[B]): IO[B] = {
      run()
      another
    }

    def as[B](newValue: => B): IO[B] = IO(newValue)

    def void: IO[Unit] = IO({
      run()
      ()
    })

    def attempt: IO[Either[Throwable, A]] = IO(Try(run()).toEither)

    def option: IO[Option[A]] = IO(Option(run()))

    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = Try(run()).fold(f, a => IO(a))

    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = IO(Try(run()).fold(recover, map))

    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = Try(run()).fold(recover, bind)

    def unsafeRunSync(): A = run()

    def unsafeToFuture()(implicit ec: ExecutionContext): Future[A] = Future(run())
  }

  object IO {
    def apply[A](body: => A): IO[A] = new IO(() => body)

    def suspend[A](thunk: => IO[A]): IO[A] = thunk

    def delay[A](body: => A): IO[A] = IO(body)

    def pure[A](a: A): IO[A] = IO(a)

    def fromEither[A](e: Either[Throwable, A]): IO[A] = e.fold(raiseError, pure)

    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option.fold[IO[A]](raiseError(orElse))(pure)

    def fromTry[A](t: Try[A]): IO[A] = t.fold(raiseError, pure)

    def none[A]: IO[Option[A]] = IO(None)

    def raiseError[A](e: Throwable): IO[A] = IO(throw e)

    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = raiseWhen(!cond)(e)

    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = if (cond) raiseError(e) else unit

    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = whenA(!cond)(action)

    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) action else unit

    val unit: IO[Unit] = IO(())
  }
}
