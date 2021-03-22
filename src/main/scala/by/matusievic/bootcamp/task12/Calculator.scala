package by.matusievic.bootcamp.task12

import by.matusievic.bootcamp.task12.Calculator._


case class Calculator(memory: Int = 0, screen: Int = 0, operation: Option[Operation] = None) {

  def enter(digit: Int): Either[String, Calculator] =
    if (digit >= 0 && digit <= 9) {
      Right(this.copy(memory = memory, screen = screen * 10 + digit, operation = operation))
    } else {
      Left("digit out of range")
    }

  def plus: Calculator = this.copy(operation = Some(Operation.Plus))

  def minus: Calculator = this.copy(operation = Some(Operation.Minus))

  def multiply : Calculator = this.copy(operation = Some(Operation.Multiply))

  def divide : Calculator = this.copy(operation = Some(Operation.Divide))

  def calculate: Calculator = operation.fold(this) {
    case Operation.Plus => Calculator(memory = memory + screen)
    case Operation.Minus => Calculator(memory = memory - screen)
    case Operation.Multiply => Calculator(memory = memory * screen)
    case Operation.Divide => Calculator(memory = memory / screen)
  }

}
object Calculator {
  sealed trait Operation
  object Operation {
    object Plus extends Operation
    object Minus extends Operation
    object Multiply extends Operation
    object Divide extends Operation
  }
}