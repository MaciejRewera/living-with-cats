package rewera.livingwithcats.monads.eithers

import cats.syntax.either._

import scala.util.Try

object EitherMain {

  def main(args: Array[String]): Unit = {
    val tryEither = Either.fromTry(Try("foo".toInt))
    val optionEither = Either.fromOption(None, "Badness")

    println(tryEither)
    println(optionEither)
    println(Either.fromOption(Some(13), "Badness"))
    println(Either.fromOption(Option(13), "Badness"))
  }
}
