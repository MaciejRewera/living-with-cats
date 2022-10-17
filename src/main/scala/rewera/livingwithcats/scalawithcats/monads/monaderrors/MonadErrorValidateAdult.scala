package rewera.livingwithcats.scalawithcats.monads.monaderrors

import cats.MonadError
import cats.instances.try_._
import cats.syntax.applicative._
import cats.syntax.applicativeError._

import scala.util.Try

object MonadErrorValidateAdult {
  def main(args: Array[String]): Unit = {
    type ExceptionOr[A] = Either[Throwable, A]

    println(validateAdult[Try](18))
    println(validateAdult[Try](8))
    println(validateAdult[ExceptionOr](-1))
    println()
    println(validateAdult2[Try](18))
    println(validateAdult2[Try](8))
    println(validateAdult2[ExceptionOr](-1))

  }

  private def validateAdult[F[_]](age: Int)(implicit monadError: MonadError[F, Throwable]): F[Int] =
    monadError.ensure(age.pure[F])(new IllegalArgumentException("Age must be greater than or equal to 18"))(_ >= 18)

  private def validateAdult2[F[_]](age: Int)(implicit monadError: MonadError[F, Throwable]): F[Int] =
    if (age >= 18) age.pure[F]
    else new IllegalArgumentException("Age must be greater than or equal to 18").raiseError[F, Int]

}
