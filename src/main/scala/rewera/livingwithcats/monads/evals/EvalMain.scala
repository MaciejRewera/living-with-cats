package rewera.livingwithcats.monads.evals

import cats.Eval

import scala.math.BigInt

object EvalMain {
  def main(args: Array[String]): Unit =
    evaluationTypes()

//    val result = factorial(50000).memoize
//    println(result.value)
//    println(result.value)

  private def evaluationTypes(): Unit = {
    val greetingAlways = Eval.always {
      println("Step 1")
      "Hello"
    }.map { str =>
      println("Step 2")
      s"$str World!"
    }
    println(greetingAlways.value)
    println(greetingAlways.value)
    println()

    val greetingLater = Eval.later {
      println("Step 1")
      "Hello"
    }.map { str =>
      println("Step 2")
      s"$str World!"
    }
    println(greetingLater.value)
    println(greetingLater.value)
    println()

    val greetingNow = Eval.now {
      println("Step 1")
      "Hello"
    }.map { str =>
      println("Step 2")
      s"$str World!"
    }.memoize
    println(greetingNow.value)
    println(greetingNow.value)
    println()

    val greetingNow2 = Eval.now {
      println("Step 1")
      "Hello"
    }.flatMap { str =>
      Eval.now {
        println("Step 2")
        s"$str World!"
      }
    }
    println(greetingNow2.value)
    println(greetingNow2.value)
  }

  private def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) Eval.now(n)
    else Eval.defer(factorial(n - 1).map(_ * n))

}
