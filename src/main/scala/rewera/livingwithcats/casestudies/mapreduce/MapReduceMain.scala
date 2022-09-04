package rewera.livingwithcats.casestudies.mapreduce

import cats.Monoid
import cats.syntax.all._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, FiniteDuration, NANOSECONDS}
import scala.concurrent.{Await, Future}

object MapReduceMain {
  private lazy val benchmarkInput = (1 to 100000000).toVector

  def main(args: Array[String]): Unit = {
    println(foldMap(Vector(1, 2, 3))(identity))
    println(foldMap(Vector(1, 2, 3))(_.toString + "! "))
    println(foldMap("Hello, world!".toVector)(_.toString.toUpperCase))

    println(Await.result(parallelFoldMap(Vector(1, 2, 3))(identity), 1.second))
    println(Await.result(parallelFoldMap(Vector(1, 2, 3))(_.toString + "! "), 1.second))
    println(Await.result(parallelFoldMap("Hello, world!".toVector)(_.toString.toUpperCase), 1.second))

    println(Await.result(parallelFoldMapWithCats(Vector(1, 2, 3))(identity), 1.second))
    println(Await.result(parallelFoldMapWithCats(Vector(1, 2, 3))(_.toString + "! "), 1.second))
    println(Await.result(parallelFoldMapWithCats("Hello, world!".toVector)(_.toString.toUpperCase), 1.second))

    runBenchmarks()
  }

  private def foldMap[A, B: Monoid](input: Vector[A])(func: A => B): B = input.foldLeft(Monoid[B].empty)(_ |+| func(_))

  private def parallelFoldMap[A, B: Monoid](input: Vector[A])(func: A => B): Future[B] = {
    val availableCpus = Runtime.getRuntime.availableProcessors()
    val groupSize = input.size / availableCpus + 1
    val groups = input.grouped(groupSize)

    foldMap(groups.toVector)(group => Future(foldMap(group)(func)))
  }

  private def parallelFoldMapWithCats[A, B: Monoid](input: Vector[A])(func: A => B): Future[B] = {
    val availableCpus = Runtime.getRuntime.availableProcessors()
    val groupSize = input.size / availableCpus + 1
    val groups = input.grouped(groupSize)

    groups.toVector.foldMap(group => Future(group.foldMap(func)))
  }

  private def runBenchmarks(): Unit = {
    println("\n--- runBenchmarks ---")

    print("foldMap...")
    val result1 = measureExecTime(foldMap(benchmarkInput)(identity))
    println(s" -> ${result1._1}")
    println(s"Execution time: ${result1._2.toMillis} ms")

    print("parallelFoldMap...")
    val result2 = measureExecTime(Await.result(parallelFoldMap(benchmarkInput)(identity), 5.seconds))
    println(s" -> ${result2._1}")
    println(s"Execution time: ${result2._2.toMillis} ms")

    print("parallelFoldMapWithCats...")
    val result3 = measureExecTime(Await.result(parallelFoldMapWithCats(benchmarkInput)(identity), 5.seconds))
    println(s" -> ${result3._1}")
    println(s"Execution time: ${result3._2.toMillis} ms")
  }

  private def measureExecTime[A](func: => A): (A, FiniteDuration) = {
    val startTime = System.nanoTime()
    val result = func
    val endTime = System.nanoTime()
    val executionTime = FiniteDuration(endTime - startTime, NANOSECONDS)

    (result, executionTime)
  }
}
