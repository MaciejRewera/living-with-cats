package rewera.livingwithcats.monads.states

import cats.data.State
import cats.data.State._

object StateMain {

  def main(args: Array[String]): Unit = {
    testComposingStates2()
    testComposingStates()
  }

  private def testComposingStates(): Unit = {
    val step1 = State[Int, String] { oldStateValue =>
      val newStateValue = oldStateValue + 1
      val result = s"New state after step1: $newStateValue"
      (newStateValue, result)
    }
    val step2 = State[Int, String] { oldStateValue =>
      val newStateValue = oldStateValue * 2
      val result = s"New state after step2: $newStateValue"
      (newStateValue, result)
    }

    val both = for {
      a <- step1
      b <- step2
    } yield (a, b)

    val both2 = step1.flatMap(a => step2.map(b => (a, b)))

    val both3 = for {
      _ <- modify[Int](_ + 1)
      a <- inspect[Int, String](i => s"New state after step1: $i")
      _ <- modify[Int](_ * 2)
      b <- inspect[Int, String](i => s"New state after step2: $i")
    } yield (a, b)

    val both4 = modify[Int](_ + 1)
      .flatMap(_ =>
        inspect[Int, String](i => s"New state after step1: $i")
          .flatMap(a =>
            modify[Int](_ * 2)
              .flatMap(_ => inspect[Int, String](i => s"New state after step2: $i"))
              .map(b => (a, b))
          )
      )

    val (state, result) = both.run(20).value
    println(state)
    println(result)
    val (state2, result2) = both2.run(20).value
    println(state2)
    println(result2)
    val (state3, result3) = both3.run(20).value
    println(state3)
    println(result3)
    val (state4, result4) = both4.run(20).value
    println(state4)
    println(result4)
  }

  private def testComposingStates2(): Unit = {
    val program = for {
      a <- get[Int]
      a1 <- set[Int](a + 1)
      b <- get[Int]
      b1 <- modify[Int](_ + 1)
      c <- inspect[Int, String](_.toString)
    } yield (a, a1, b, b1, c)

    val (state, result) = program.run(1).value
    println(state)
    println(result)
  }

}
