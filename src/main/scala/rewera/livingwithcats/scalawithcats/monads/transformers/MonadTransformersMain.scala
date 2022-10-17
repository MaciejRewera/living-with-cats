package rewera.livingwithcats.scalawithcats.monads.transformers

import cats.data.EitherT

import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.successful
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Await, Future}

object MonadTransformersMain {

  type Response[A] = EitherT[Future, String, A]

  private val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  private def getPowerLevel(autobot: String): Response[Int] = powerLevels.get(autobot) match {
    case Some(powerLevel) => EitherT.right(successful(powerLevel))
    case None             => EitherT.left(successful(s"Autobot $autobot is unreachable."))
  }

  private def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = for {
    powerAlly1 <- getPowerLevel(ally1)
    powerAlly2 <- getPowerLevel(ally2)
  } yield (powerAlly1 + powerAlly2) >= 15

  private def tacticalReport(ally1: String, ally2: String): String = waitForResponse(canSpecialMove(ally1, ally2)) match {
    case Left(errorMsg) => s"Comms error: $errorMsg"
    case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
    case Right(false) => s"$ally1 and $ally2 need a recharge"
  }

  def main(args: Array[String]): Unit = {

    println("Power levels:")
    println(s"Jazz - ${waitForResponse(getPowerLevel("Jazz"))}")
    println(s"Bumblebee - ${waitForResponse(getPowerLevel("Bumblebee"))}")
    println(s"Hot Rod - ${waitForResponse(getPowerLevel("Hot Rod"))}")
    println(s"Ironhide - ${waitForResponse(getPowerLevel("Ironhide"))}")

    println("\nCan do special move?")
    println(s"Jazz + Bumblebee: ${waitForResponse(canSpecialMove("Jazz", "Bumblebee"))}")
    println(s"Jazz + Hot Rod: ${waitForResponse(canSpecialMove("Jazz", "Hot Rod"))}")
    println(s"Bumblebee + Hot Rod: ${waitForResponse(canSpecialMove("Bumblebee", "Hot Rod"))}")
    println(s"Ironhide + Hot Rod: ${waitForResponse(canSpecialMove("Ironhide", "Bumblebee"))}")

    println("\nTactical reports:")
    println(tacticalReport("Jazz", "Bumblebee"))
    println(tacticalReport("Jazz", "Hot Rod"))
    println(tacticalReport("Bumblebee", "Hot Rod"))
    println(tacticalReport("Ironhide", "Bumblebee"))

  }

  private val maxWaitTime: FiniteDuration = Duration(1, TimeUnit.SECONDS)
  private def waitForResponse[A](func: => Response[A]): Either[String, A] = Await.result(func.value, maxWaitTime)
}
