package by.matusievic.bootcamp.task10

import java.time.LocalDate
import scala.util.Try

object Homework {
  case class CardNumber(underlying: Long) extends AnyVal
  case class SecurityCode(underlying: Int) extends AnyVal
  case class CardHolder(name: String, surname: String)

  case class PaymentCard(number: CardNumber, holder: CardHolder, expirationDate: LocalDate, code: SecurityCode)

  sealed trait ValidationError
  object ValidationError {
    final case object CardNumberHasInvalidLength extends ValidationError {
      override def toString: String = "Card number has invalid length"
    }
    final case object CardNumberContainsNonDigitCharacters extends ValidationError {
      override def toString: String = "Card number contains non digit characters"
    }
    final case object CardHolderHasInvalidFormat extends ValidationError {
      override def toString: String = "Card holder doesn't meet format: 'Name Surname'"
    }
    final case object CardExpirationDateIsInvalid extends ValidationError {
      override def toString: String = "Card expiration date doesn't meet format: 'YYYY-MM-DD'"
    }
    final case object CardSecurityCodeHasInvalidLength extends ValidationError {
      override def clone(): AnyRef = "Card security code has invalid length"
    }
    final case object CardSecurityCodeContainsNonDigitCharacters extends ValidationError {
      override def toString: String = "Card security code contains non digit characters"
    }
  }

  object PaymentCardValidator {

    import ValidationError._
    import cats.data._
    import cats.implicits._

    private val CardNumberLength = 16
    private val MinSecurityCodeLength = 3
    private val MaxSecurityCodeLength = 4

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate(
                  holder: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] = {
      def validateHolder(holder: String): AllErrorsOr[CardHolder] = {
        def isNameLike(s: String): Boolean = s.length >= 2 && s.head.isUpper && s.tail.forall(_.isLower)

        holder.split(" ").toList match {
          case name :: surname :: Nil if isNameLike(name) && isNameLike(surname) =>
            CardHolder(name, surname).validNec
          case _ =>
            CardHolderHasInvalidFormat.invalidNec
        }
      }

      def validateNumber(number: String): AllErrorsOr[CardNumber] = {
        def validateNumberLength(number: String): AllErrorsOr[String] = {
          if (number.length == CardNumberLength) {
            number.validNec
          } else {
            CardNumberHasInvalidLength.invalidNec
          }
        }

        def validateNumberContent(number: String): AllErrorsOr[CardNumber] = {
          if (number.forall(_.isDigit)) {
            CardNumber(number.toLong).validNec
          } else {
            CardNumberContainsNonDigitCharacters.invalidNec
          }
        }

        validateNumberLength(number).andThen(validateNumberContent)
      }

      def validateExpirationDate(date: String): AllErrorsOr[LocalDate] = {
        Try(LocalDate.parse(date)).fold(_ => CardExpirationDateIsInvalid.invalidNec, _.validNec)
      }

      def validateSecurityCode(code: String): AllErrorsOr[SecurityCode] = {
        def validateSecurityCodeLength(code: String): AllErrorsOr[String] = {
          if (code.length >= MinSecurityCodeLength && code.length <= MaxSecurityCodeLength) {
            code.validNec
          } else {
            CardSecurityCodeHasInvalidLength.invalidNec
          }
        }

        def validateSecurityCodeContent(code: String): AllErrorsOr[SecurityCode] = {
          if (code.forall(_.isDigit)) {
            SecurityCode(code.toInt).validNec
          } else {
            CardSecurityCodeContainsNonDigitCharacters.invalidNec
          }
        }

        validateSecurityCodeLength(code).andThen(validateSecurityCodeContent)
      }

      (validateNumber(number),
        validateHolder(holder),
        validateExpirationDate(expirationDate),
        validateSecurityCode(securityCode)).mapN(PaymentCard)
    }
  }
}
