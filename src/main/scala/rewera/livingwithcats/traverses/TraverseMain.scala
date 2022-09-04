package rewera.livingwithcats.traverses

import cats.Applicative
import cats.data.Validated
import cats.syntax.applicative._
import cats.syntax.apply._

object TraverseMain {

  def main(args: Array[String]): Unit = {
    println(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))))

    println(process(List(2, 4, 6)))
    println(process(List(1, 2, 3)))

    println(processValidated(List(2, 4, 6)))
    println(processValidated(List(1, 2, 3)))
  }

  private def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F])((acc, item) => (acc, func(item)).mapN(_ :+ _))

  private def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = listTraverse(list)(identity)

  private def process(input: List[Int]): Option[List[Int]] = listTraverse(input)(n => if (n % 2 == 0) Some(n) else None)

  private type ErrorsOr[A] = Validated[List[String], A]

  private def processValidated(input: List[Int]): ErrorsOr[List[Int]] = listTraverse(input) { n =>
    if (n % 2 == 0)
      Validated.valid(n)
    else
      Validated.invalid(List(s"$n is not even"))
  }
}
