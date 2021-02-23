package by.matusievic.bootcamp.task3

import scala.io.Source

object ControlStructuresHomework {

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }
  final case class ErrorMessage(value: String)
  object ErrorMessage {
    val NotNumberInput: ErrorMessage = ErrorMessage("Not number import provided")
    val DivisorIsZero: ErrorMessage = ErrorMessage("Divisor is zero")
    val OperandsAreMissing: ErrorMessage = ErrorMessage("Operands are missing")
    val InvalidCommand: ErrorMessage = ErrorMessage("Invalid command")
  }

  import by.matusievic.bootcamp.task3.ControlStructuresHomework.Command._

  sealed trait Result
  final case class DivideResult(result: Double, command: Divide) extends Result
  final case class SumResult(result: Double, command: Sum) extends Result
  final case class AverageResult(result: Double, command: Average) extends Result
  final case class MinResult(result: Double, command: Min) extends Result
  final case class MaxResult(result: Double, command: Max) extends Result

  import Command._

  def parseCommand(x: String): Either[ErrorMessage, Command] = x.split(" ").filter(_.nonEmpty).toList match {
    case "divide" :: dividend :: divisor :: Nil =>
      for {
        dividend <- dividend.toDoubleOption.toRight(ErrorMessage.NotNumberInput)
        divisor <- divisor.toDoubleOption.toRight(ErrorMessage.NotNumberInput)
      } yield Divide(dividend, divisor)
    case "sum" :: numbers => parseMultipleNumbersCommand(numbers, Sum)
    case "average" :: numbers => parseMultipleNumbersCommand(numbers, Average)
    case "min" :: numbers => parseMultipleNumbersCommand(numbers, Min)
    case "max" :: numbers => parseMultipleNumbersCommand(numbers, Max)
    case _ => Left(ErrorMessage.InvalidCommand)
  }

  def parseMultipleNumbersCommand(numbers: List[String], constructor: List[Double] => Command): Either[ErrorMessage, Command] = {
    val numberOptions = numbers.map(_.toDoubleOption)
    Either.cond(!numberOptions.contains(None), constructor(numberOptions.flatten), ErrorMessage.NotNumberInput)
  }

  def calculate(x: Command): Either[ErrorMessage, Result] = x match {
    case c@Divide(dividend, divisor) => Either.cond(divisor != 0, DivideResult(dividend / divisor, c), ErrorMessage.DivisorIsZero)
    case c@Sum(numbers) => Either.cond(numbers.nonEmpty, SumResult(numbers.sum, c), ErrorMessage.OperandsAreMissing)
    case c@Average(numbers) => Either.cond(numbers.nonEmpty, AverageResult(numbers.sum / numbers.size, c), ErrorMessage.OperandsAreMissing)
    case c@Min(numbers) => Either.cond(numbers.nonEmpty, MinResult(numbers.min, c), ErrorMessage.OperandsAreMissing)
    case c@Max(numbers) => Either.cond(numbers.nonEmpty, MaxResult(numbers.max, c), ErrorMessage.OperandsAreMissing)
  }

  def renderResult(x: Result): String = x match {
    case DivideResult(result, Divide(dividend, divisor)) => s"$dividend divided by $divisor is $result"
    case SumResult(result, Sum(numbers)) => s"the sum of ${numbers.mkString(" ")} is $result"
    case AverageResult(result, Average(numbers)) => s"the average of ${numbers.mkString(" ")} is $result"
    case MinResult(result, Min(numbers)) => s"the minimum of ${numbers.mkString(" ")} is $result"
    case MaxResult(result, Max(numbers)) => s"the maximum of ${numbers.mkString(" ")} is $result"
  }

  def process(x: String): String = {
    val errorOrResult = for {
      cmd <- parseCommand(x)
      res <- calculate(cmd)
    } yield res
    errorOrResult.fold(e => s"Error: ${e.value}", renderResult)
  }

  def main(args: Array[String]): Unit =
    for (
      str <- Source.stdin.getLines() map { line =>
        process(line)
      }
    ) println(str)

}
