package by.matusievic.bootcamp.task10

import by.matusievic.bootcamp.task10.Homework.ValidationError._
import by.matusievic.bootcamp.task10.Homework._
import cats.data.Chain
import cats.data.Validated.{Invalid, Valid}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.LocalDate

class HomeworkTest extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "validate" should "return valid" in {
    PaymentCardValidator.validate(
      holder = "John Doe",
      number = "1234567812345678",
      expirationDate = "2020-01-01",
      securityCode = "1234"
    ) shouldBe Valid(PaymentCard(
      CardNumber(1234567812345678L),
      CardHolder("John", "Doe"),
      LocalDate.of(2020, 1, 1),
      SecurityCode(1234)
    ))
  }

  "validate" should "catch invalid name" in {
    PaymentCardValidator.validate(
      holder = "JohnDoe",
      number = "1234567812345678",
      expirationDate = "2020-01-01",
      securityCode = "1234"
    ) shouldBe Invalid(Chain(CardHolderHasInvalidFormat))
  }

  "validate" should "catch invalid number length" in {
    PaymentCardValidator.validate(
      holder = "John Doe",
      number = "12345678123456780",
      expirationDate = "2020-01-01",
      securityCode = "1234"
    ) shouldBe Invalid(Chain(CardNumberHasInvalidLength))
  }

  "validate" should "catch non-digit number" in {
    PaymentCardValidator.validate(
      holder = "John Doe",
      number = "123456781234567A",
      expirationDate = "2020-01-01",
      securityCode = "1234"
    ) shouldBe Invalid(Chain(CardNumberContainsNonDigitCharacters))
  }

  "validate" should "catch non-parsable date" in {
    PaymentCardValidator.validate(
      holder = "John Doe",
      number = "1234567812345678",
      expirationDate = "abc",
      securityCode = "1234"
    ) shouldBe Invalid(Chain(CardExpirationDateIsInvalid))
  }

  "validate" should "catch invalid code length" in {
    PaymentCardValidator.validate(
      holder = "John Doe",
      number = "1234567812345678",
      expirationDate = "2020-01-01",
      securityCode = "12345"
    ) shouldBe Invalid(Chain(CardSecurityCodeHasInvalidLength))
  }

  "validate" should "catch non-digit code characters" in {
    PaymentCardValidator.validate(
      holder = "John Doe",
      number = "1234567812345678",
      expirationDate = "2020-01-01",
      securityCode = "abc"
    ) shouldBe Invalid(Chain(CardSecurityCodeContainsNonDigitCharacters))
  }

  "validate" should "aggregate all errors" in {
    PaymentCardValidator.validate(
      holder = "abc",
      number = "abc",
      expirationDate = "abc",
      securityCode = "abc"
    ) shouldBe Invalid(Chain(
      CardNumberHasInvalidLength,
      CardHolderHasInvalidFormat,
      CardExpirationDateIsInvalid,
      CardSecurityCodeContainsNonDigitCharacters
    ))
  }
}
