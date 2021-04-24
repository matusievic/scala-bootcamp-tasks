package by.matusievic.bootcamp.task19

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import io.circe.{Codec, Decoder, Encoder}
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.dsl.io.{->, /, Ok, Root}
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.{HttpRoutes, ResponseCookie, _}
import org.http4s.dsl.io._

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.Random

// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// The exact protocol and message format to use is not specified and should be designed while working on the task.

import io.circe.generic.semiauto._
import org.http4s.circe.CirceEntityCodec._

object protocol {

  final case class InitGameReq(from: Int, to: Int)
  final case class InitGameResp(uuid: String)

  final case class GuessAttemptReq(number: Int)
  final case class GuessAttemptResp(result: GuessAttemptResult)

  sealed trait GuessAttemptResult {
    def value: String
  }
  object GuessAttemptResult {
    val all = Seq(Lower, Greater, Guessed, NoMoreAttempts)

    def fromString(str: String): Option[GuessAttemptResult] = all.find(_.value == str)
  }

  case object Lower extends GuessAttemptResult {
    override def value: String = "lower"
  }
  case object Greater extends GuessAttemptResult {
    override def value: String = "greater"
  }
  case object Guessed extends GuessAttemptResult {
    override def value: String = "guessed"
  }
  case object NoMoreAttempts extends GuessAttemptResult {
    override def value: String = "no more attempts"
  }

  implicit val codecInitGameReq: Codec[InitGameReq] = deriveCodec[InitGameReq]
  implicit val codecInitGameResp: Codec[InitGameResp] = deriveCodec[InitGameResp]
  implicit val codecGuessAttemptReq: Codec[GuessAttemptReq] = deriveCodec[GuessAttemptReq]
  implicit val codecGuessAttemptResp: Codec[GuessAttemptResp] = deriveCodec[GuessAttemptResp]
  implicit val codecGuessAttemptResult: Codec[GuessAttemptResult] = Codec.from(Decoder.decodeString.emap(s => GuessAttemptResult.fromString(s).toRight(s)), Encoder.encodeString.contramap(_.value))
}


object GuessServer extends IOApp {

  import protocol._

  private val MaxAttempts = 5
  private val sessions: mutable.Map[String, Int] = mutable.Map.empty
  private val attempts: mutable.Map[String, Int] = mutable.Map.empty

  private val routes = HttpRoutes.of[IO] {
    case req@POST -> Root / "play" =>
      req.as[InitGameReq].flatMap { body =>
        val uuid = java.util.UUID.randomUUID.toString
        sessions(uuid) = Random.between(body.from, body.to)
        attempts(uuid) = MaxAttempts
        Ok(InitGameResp(uuid)).map(_.addCookie(ResponseCookie("uuid", uuid)))
      }
    case req@POST -> Root / "guess" =>
      req.as[GuessAttemptReq].flatMap { body =>
        req.cookies.find(_.name == "uuid") match {
          case Some(RequestCookie(_, uuid)) if sessions.keySet.contains(uuid) =>
            val guessedNumber = sessions(uuid)
            val attemptsAllowed = attempts(uuid)

            if (guessedNumber == body.number) {
              sessions -= uuid
              attempts -= uuid
              Ok(GuessAttemptResp(Guessed))
            } else if (attemptsAllowed > 1) {
              attempts(uuid) = attemptsAllowed - 1
              if (guessedNumber < body.number) {
                Ok(GuessAttemptResp(Lower))
              } else {
                Ok(GuessAttemptResp(Greater))
              }
            } else {
              Ok(GuessAttemptResp(NoMoreAttempts))
            }
          case None => BadRequest("There's such session")
        }
      }
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(routes)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}

object GuessClient extends IOApp {

  import cats.effect._
  import org.http4s._
  import protocol._

  private val uri = uri"http://localhost:9001"

  private def halfDistance(from: Int, to: Int) = (to - from) / 2

  case class GameResult(number: Option[Int]) {
    override def toString: String = number.fold("You lost.")(n => s"You win. Number is $n")
  }

  private def play(from: Int, to: Int)(implicit client: Client[IO]): IO[String] = {
    client.expect[InitGameResp] {
      Request[IO](method = Method.POST, uri = uri / "play").withEntity(InitGameReq(from, to))
    }.map { case InitGameResp(uuid) => uuid }
  }

  private def printLine(s: String): IO[Unit] = IO(println(s))

  private def guessAttempt(from: Int, to: Int)(implicit client: Client[IO], uuid: String): IO[GameResult] = {
    val current = from + halfDistance(from, to)
    client.expect[GuessAttemptResp] {
      Request[IO](method = Method.POST, uri = uri / "guess")
        .withEntity(GuessAttemptReq(current))
        .addCookie("uuid", uuid)
    }.flatMap {
      case GuessAttemptResp(Lower) =>
        printLine(s"Number is lower than $current. Trying one more time.") >> guessAttempt(from, current)
      case GuessAttemptResp(Greater) =>
        printLine(s"Number is greater than $current. Trying one more time.") >> guessAttempt(current, to)
      case GuessAttemptResp(Guessed) =>
        printLine(s"You win. Number is $current") >> IO(GameResult(Some(current)))
      case GuessAttemptResp(NoMoreAttempts) =>
        printLine("You lost. No more attempts.") >> IO(GameResult(None))
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val (from, to) = (1, 100)
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .parZip(Blocker[IO]).use { case (client, _) =>
      for {
        uuid <- play(from, to)(client)
        _ <- guessAttempt(from, to)(client, uuid)
      } yield ()
    }.as(ExitCode.Success)
  }
}

