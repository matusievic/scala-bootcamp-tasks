package by.matusievic.bootcamp.task11

import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import io.circe
import io.circe.parser._
import org.scalatest.EitherValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scalaj.http.Http

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.io.Source
import scala.util.Try

class HomeworkSpec extends AnyWordSpec with Matchers with EitherValues {

  import HomeworkSpec._

  "NBA JSON API client" should {
    "get info about today games" in {
      val date = LocalDate.now()
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      succeed
    }

    "fetch games for 14 Feb 2020" in {
      val date = LocalDate.of(2020, 2, 14)
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      val gameInfos = gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      gameInfos.size must be(1)
    }
  }

}

object HomeworkSpec {
  private def fetchScoreboard(date: LocalDate): Either[circe.Error, Scoreboard] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val jsonString = Try {
      val src = Source.fromResource(s"scoreboard_$dateString.json")
      val s = src.mkString
      src.close()
      s
    }.getOrElse {
      val url = s"https://data.nba.net/10s/prod/v1/$dateString/scoreboard.json"
      Http(url).asString.body
    }
    decode[Scoreboard](jsonString)
  }

  private def fetchGameInfo(date: LocalDate, gameId: String): Either[circe.Error, BoxScore] = {
    val jsonString = Try {
      val src = Source.fromResource(s"${gameId}_boxscore.json")
      val s = src.mkString
      src.close()
      s
    }.getOrElse {
      val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
      val url = s"https://data.nba.net/10s/prod/v1/$dateString/${gameId}_boxscore.json"
      Http(url).asString.body
    }
    decode[BoxScore](jsonString)
  }
}
