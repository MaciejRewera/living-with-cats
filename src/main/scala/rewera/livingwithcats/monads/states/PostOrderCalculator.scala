package rewera.livingwithcats.monads.states

import cats.data.State
import cats.data.State._

object PostOrderCalculator {

  type CalcState[A] = State[List[Int], A]

  private val mathSymbols = Seq("+", "-", "*", "/")

  def evalOne(symbol: String): CalcState[Int] = State[List[Int], Int] { oldStack =>
    if (mathSymbols.contains(symbol)) {
      val num1 +: num2 +: tmpStack = oldStack
      val newResult = calc(num1, num2, symbol)
      val newStack = newResult +: tmpStack
      (newStack, newResult)

    } else {
      val newStack = symbol.toInt +: oldStack
      (newStack, symbol.toInt)
    }
  }

  def evalOne2(symbol: String): CalcState[Int] = if (mathSymbols.contains(symbol)) {
    for {
      newResult <- inspect[List[Int], Int](calc(_, symbol))
      _ <- modify[List[Int]](newResult +: _.drop(2))
    } yield newResult

  } else {
    for {
      _ <- modify[List[Int]](symbol.toInt +: _)
      newResult <- pure[List[Int], Int](symbol.toInt)
    } yield newResult
  }

  private def calc(stack: List[Int], symbol: String): Int = {
    val num1 +: num2 +: _ = stack
    calc(num1, num2, symbol)
  }

  private def calc(num1: Int, num2: Int, operation: String): Int = operation match {
    case "+" => num1 + num2
    case "-" => num1 - num2
    case "*" => num1 * num2
    case "/" => num1 / num2
  }

  def main(args: Array[String]): Unit = {
    println(evalOne("42").runA(Nil).value)

    val simpleProgram = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      _ <- evalOne("+")
      _ <- evalOne("3")
      answer <- evalOne("*")
    } yield answer
    println(simpleProgram.runA(Nil).value)

    val simpleProgram2 = for {
      _ <- evalOne2("1")
      _ <- evalOne2("2")
      _ <- evalOne2("+")
      _ <- evalOne2("3")
      answer <- evalOne2("*")
    } yield answer
    println(simpleProgram2.runA(Nil).value)

  }
}
