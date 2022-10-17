package rewera.livingwithcats.scalawithcats.monads.writers

import cats.data.Writer
import cats.implicits.catsSyntaxWriterId
import cats.instances.vector._
import cats.syntax.applicative._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.Random

object WriterMain {

  def main(args: Array[String]): Unit = {

    val writer1 = for {
      a <- 10.pure[Logged]
      _ <- Vector("a", "b","c").tell
      b <- 32.writer(Vector("d", "e", "f"))
    } yield a + b
    println(writer1.run)

    val writer2 = 10.pure[Logged]
      .flatMap(a => Writer(Vector("a", "b","c"), a).flatMap(_ => Writer(Vector("d", "e", "f"), 32).map(b => a + b)))

    println(writer2.run)

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
