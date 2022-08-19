package rewera.livingwithcats.monads.states

import cats.data.State
import cats.data.State._
import cats.implicits.catsSyntaxApplicativeId

object PostOrderCalculator {

  type CalcState[A] = State[Seq[Int], A]

  private val mathSymbols = Seq("+", "-", "*", "/")

  def runInput(input: String): Int = evalInput(input).runA(Nil).value

  def evalInput(input: String): CalcState[Int] = evalAll(input.split("\\x20+").toSeq)

  def evalAll(input: Seq[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState])((stack, symbol) => stack.flatMap(_ => evalOne(symbol)))

  def evalOne(symbol: String): CalcState[Int] = State[Seq[Int], Int] { oldStack =>
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
      newResult <- inspect[Seq[Int], Int](calc(_, symbol))
      _ <- modify[Seq[Int]](newResult +: _.drop(2))
    } yield newResult

  } else {
    for {
      _ <- modify[Seq[Int]](symbol.toInt +: _)
      newResult <- pure[Seq[Int], Int](symbol.toInt)
    } yield newResult
  }

  private def calc(stack: Seq[Int], symbol: String): Int = {
    val num1 +: num2 +: _ = stack
    calc(num1, num2, symbol)
  }

  private def calc(num1: Int, num2: Int, operation: String): Int = operation match {
    case "+" => num1 + num2
    case "-" => num1 - num2
    case "*" => num1 * num2
    case "/" => num1 / num2
  }

  def evalOne3(symbol: String): CalcState[Int] = symbol match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num.toInt)
  }

  private def operand(num: Int): CalcState[Int] = State[Seq[Int], Int](oldStack => (num +: oldStack, num))

  private def operator(func: (Int, Int) => Int): CalcState[Int] = State[Seq[Int], Int] {
    case num1 :: num2 :: tmpStack =>
      val res = func(num1, num2)
      (res :: tmpStack, res)

    case _ => throw new IllegalStateException
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
    println("simpleProgram result = " + simpleProgram.runA(Nil).value)

    val simpleProgram2 = for {
      _ <- evalOne2("1")
      _ <- evalOne2("2")
      _ <- evalOne2("+")
      _ <- evalOne2("3")
      answer <- evalOne2("*")
    } yield answer
    println("simpleProgram2 result = " + simpleProgram2.runA(Nil).value)

    val simpleProgram3 = for {
      _ <- evalOne3("1")
      _ <- evalOne3("2")
      _ <- evalOne3("+")
      _ <- evalOne3("3")
      answer <- evalOne3("*")
    } yield answer
    println("simpleProgram3 result = " + simpleProgram3.runA(Nil).value)

    /** *****************************************************************************************
      */
    val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))
    println("multistageProgram result = " + multistageProgram.runA(Nil).value)

    val biggerProgram = for {
      _ <- evalAll(List("1", "2", "+"))
      _ <- evalAll(List("3", "4", "+"))
      ans <- evalOne("*")
    } yield ans
    println("biggerProgram result = " + biggerProgram.runA(Nil).value)

    /** *****************************************************************************************
      */
    val multistageProgramString = "1 2 + 3 * "
    val multistageConvenientToReadProgram = evalInput(multistageProgramString)
    println("multistageConvenientToReadProgram result = " + multistageConvenientToReadProgram.runA(Nil).value)
    println("runInput(multistageProgramString) result = " + runInput(multistageProgramString))

    val biggerProgramString = "1 2 + 3 4 +  * "
    val biggerConvenientToReadProgram = evalInput(biggerProgramString)
    println("biggerConvenientToReadProgram result = " + biggerConvenientToReadProgram.runA(Nil).value)
    println("runInput(biggerProgramString) result = " + runInput(biggerProgramString))

  }
}
