package rewera.livingwithcats.monads.evals

import cats.Eval

object EvalFolding {
  def main(args: Array[String]): Unit = {
    val input = (0 to 50000).toList
    val result = foldRight(input, 0)(_ + _)

    println(result.value)
    println(result.value == input.sum)
  }

  private def foldRight[A, B](input: List[A], acc: B)(func: (A, B) => B): Eval[B] = input match {
    case Nil          => Eval.now(acc)
    case head :: tail => Eval.defer(foldRight(tail, acc)(func).map(func(head, _)))
  }
}
