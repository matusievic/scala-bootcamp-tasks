package by.matusievic.bootcamp.task11

import io.circe.generic.semiauto.deriveDecoder
import io.circe.{Decoder, JsonObject}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}
import scala.util.Try

final case class TeamTotals(assists: String, fullTimeoutRemaining: String, plusMinus: String)
object TeamTotals {
  implicit val decoderTeamTotals: Decoder[TeamTotals] = {
    def toCamelCase(s: String): String = {
      val split = s.split("_")
      val tail = split.tail.map { x => x.head.toUpper + x.tail }
      split.head + tail.mkString
    }

    deriveDecoder[TeamTotals].prepare {
      _.withFocus {
        _.mapObject { obj =>
          JsonObject(obj.toMap.collect {
            case (k, v) => toCamelCase(k) -> v
          }.toArray: _*)
        }
      }
    }
  }
}

final case class TeamBoxScore(totals: TeamTotals)
object TeamBoxScore {
  implicit val decoderTeamBoxScore: Decoder[TeamBoxScore] = deriveDecoder[TeamBoxScore]
}

final case class GameStats(hTeam: TeamBoxScore, vTeam: TeamBoxScore)
object GameStats {
  implicit val decoderGameStats: Decoder[GameStats] = deriveDecoder[GameStats]
}

final case class PrevMatchup(gameDate: LocalDate, gameId: String)
object PrevMatchup {
  private val DateFormat = DateTimeFormatter.ofPattern("yyyyMMdd")
  private implicit val decoderLocalDate: Decoder[LocalDate] = Decoder.decodeString.emapTry(s => Try(LocalDate.parse(s, DateFormat)))
  implicit val decoderPrevMatchup: Decoder[PrevMatchup] = deriveDecoder[PrevMatchup]
}

final case class BoxScore(
                           basicGameData: Game,
                           previousMatchup: PrevMatchup,
                           stats: Option[GameStats],
                         )
object BoxScore {
  implicit val decoderBoxScore: Decoder[BoxScore] = deriveDecoder[BoxScore]
}

final case class JustScore(score: String)
object JustScore {
  implicit val decoderJustScore: Decoder[JustScore] = deriveDecoder[JustScore]
}

final case class TeamStats(
                            linescore: List[JustScore],
                            loss: String,
                            score: String,
                            teamId: String,
                            triCode: String
                          )
object TeamStats {
  implicit val decoderTeamStats: Decoder[TeamStats] = deriveDecoder[TeamStats]
}

final case class GameDuration(hours: String, minutes: String)
object GameDuration {
  implicit val decoderGameDuration: Decoder[GameDuration] = deriveDecoder[GameDuration]
}

final case class Arena(
                        city: String,
                        country: String,
                        isDomestic: Boolean,
                        name: String,
                        stateAbbr: String
                      )
object Arena {
  implicit val decoderArena: Decoder[Arena] = deriveDecoder[Arena]
}

final case class Game(
                       arena: Arena,
                       attendance: String,
                       endTimeUTC: Option[ZonedDateTime],
                       gameDuration: GameDuration,
                       gameId: String,
                       gameUrlCode: String,
                       hTeam: TeamStats,
                       isBuzzerBeater: Boolean,
                       startTimeUTC: ZonedDateTime,
                       vTeam: TeamStats,
                     )
object Game {
  implicit val decoderGame: Decoder[Game] = deriveDecoder[Game]
}

final case class Scoreboard(games: List[Game], numGames: Int)
object Scoreboard {
  implicit val decoderScoreboard: Decoder[Scoreboard] = deriveDecoder[Scoreboard]
}
