package rewera.livingwithcats.monads.states

import cats.data.State

object StateMain {

  def main(args: Array[String]): Unit = {
    testComposingStates()
  }

  private def testComposingStates(): Unit = {
    val step1 = State[Int, String] { num =>
      val newStateValue = num + 1
      val result = s"New state after step1: $newStateValue"
      (newStateValue, result)
    }
    val step2 = State[Int, String] { num =>
      val newStateValue = num * 2
      val result = s"New state after step2: $newStateValue"
      (newStateValue, result)
    }

    val both = for {
      a <- step1
      b <- step2
    } yield (a, b)

    val both2 = step1.flatMap(a => step2.map((a, _)))

    val (state, result) = both.run(20).value
    println(state)
    println(result)
    val (state2, result2) = both2.run(20).value
    println(state2)
    println(result2)
  }

}
