package rewera.livingwithcats.monads.writers

import cats.data.Writer

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.Random
import cats.syntax.applicative._
import cats.instances.vector._

object WriterMain {

  def main(args: Array[String]): Unit = {

    val writerResults = Await.result(
      Future.sequence(
        Seq(
          Future(factorial(5)),
          Future(factorial(5))
        )
      ),
      5.seconds
    )

    val results = writerResults.map(_.value)
    val logs = writerResults.map(_.written)

    results.foreach(println)

    logs.flatten.foreach(println)
  }

  private def slowly[A](body: => A): A = try body
  finally Thread.sleep(1 + Random.nextLong(100))

  type Logged[A] = Writer[Vector[String], A]

  private def factorial(n: Int): Logged[Int] =
    slowly {
      if (n == 0) 1.pure[Logged]
      else
        factorial(n - 1).mapBoth { (logs, value) =>
          val result = value * n
          val newLogs = logs :+ s"factorial $n is $result"

          (newLogs, result)
        }
    }
}
