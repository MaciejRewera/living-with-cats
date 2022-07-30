package rewera.livingwithcats.monads

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Id, Monad}

object MonadMain {

  def main(args: Array[String]): Unit = {
    println(sumSquare(Some(1), Option(2)))
    println(sumSquare(Seq(1, 2, 3), Seq(4, 5)))

    println(sumSquare(3: Id[Int], Id(4)))
  }

  private def sumSquare[F[_]: Monad](first: F[Int], second: F[Int]): F[Int] =
    for {
      f <- first
      s <- second
    } yield f * f + s * s
}
