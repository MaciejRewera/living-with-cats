package rewera.livingwithcats.semigroupal

import cats.instances.int._
import cats.instances.invariant._
import cats.instances.string._
import cats.syntax.apply._
import cats.syntax.semigroup._
import cats.{Monoid, Semigroupal}
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

  }

  private def catMonoids(): Unit = {

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
    case class CatWithFood(name: String, age: Int, favoriteFoods: Seq[String])

    val futureCat = (
      Future("Garfield"),
      Future(14),
      Future(List("Lasagne"))
    ).mapN(CatWithFood.apply)

    println(Await.result(futureCat, 1.second))
  }

  private def semigroupalWithLists(): Unit =
    println((List(1, 2), List(3, 4)).tupled)

  private type ErrorOr[A] = Either[Vector[String], A]
  private def semigroupalWithEithers(): Unit =
    println(
      Semigroupal[ErrorOr].product(
        Left(Vector("Error 1")),
        Left(Vector("Error 2"))
      )
    )
}
