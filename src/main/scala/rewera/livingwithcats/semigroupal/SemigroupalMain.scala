package rewera.livingwithcats.semigroupal

import cats.Monoid
import cats.instances.int._
import cats.instances.invariant._
import cats.instances.string._
import cats.syntax.apply._
import cats.syntax.parallel._
import cats.syntax.semigroup._
import rewera.livingwithcats.models.Cat

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object SemigroupalMain {

  def main(args: Array[String]): Unit = {

    println((Option(123), Option("abc")).tupled)
    println((Option(123), Option("abc"), Option(true)).tupled)
    println(
      (
        Option("Garfield"),
        Option(13),
        Option("Orange & black")
      ).mapN(Cat.apply)
    )

    catMonoids()
    semigroupalWithFutures()
    semigroupalWithLists()
    semigroupalWithEithers()
    parallelWithEithers()

  }

  private def catMonoids(): Unit = {
    println("\n--- catMonoids() ---")

    val tupleToCat: (String, Int, String) => Cat = Cat.apply
    val catToTuple: Cat => (String, Int, String) = cat => (cat.name, cat.age, cat.color)

    implicit val catMonoid: Monoid[Cat] = (
      Monoid[String],
      Monoid[Int],
      Monoid[String]
    ).imapN(tupleToCat)(catToTuple)

    val garfield = Cat("Garfield", 13, "Orange & black")
    val heathcliff = Cat("Heathcliff", 7, "Just black")

    println(garfield |+| heathcliff)
  }

  private def semigroupalWithFutures(): Unit = {
    println("\n--- semigroupalWithFutures() ---")
    case class CatWithFood(name: String, age: Int, favoriteFoods: Seq[String])

    val futureCat = (
      Future("Garfield"),
      Future(14),
      Future(List("Lasagne"))
    ).mapN(CatWithFood.apply)

    println(Await.result(futureCat, 1.second))
  }

  private def semigroupalWithLists(): Unit = {
    println("\n--- semigroupalWithLists() ---")
    println((List(1, 2), List(3, 4)).tupled)
  }

  private type ErrorOr[A] = Either[Seq[String], A]
  private def semigroupalWithEithers(): Unit = {
    println("\n--- semigroupalWithEithers() ---")
    val error1: ErrorOr[Int] = Left(Seq("Error 1"))
    val error2: ErrorOr[Int] = Left(Seq("Error 2"))

    println((error1, error2).tupled)
  }

  private def parallelWithEithers(): Unit = {
    println("\n--- parallelWithEithers() ---")
    val error1: ErrorOr[Int] = Left(Seq("Error 1"))
    val error2: ErrorOr[Int] = Left(Seq("Error 2"))
    println((error1, error2).parTupled)

    val success1: ErrorOr[Int] = Right(1)
    val success2: ErrorOr[Int] = Right(2)
    val addTwo = (x: Int, y: Int) => x + y

    println((error1, error2).parMapN(addTwo))
    println((success1, success2).parMapN(addTwo))
  }

  private type MyType[A, B] = Option[A => B]
}
