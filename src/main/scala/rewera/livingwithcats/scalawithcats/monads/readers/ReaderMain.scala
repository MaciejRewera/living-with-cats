package rewera.livingwithcats.scalawithcats.monads.readers

import cats.data.Reader
import cats.syntax.applicative._

object ReaderMain {

  private type UserId = Int
  private type Username = String
  private type Password = String
  private type DbReader[A] = Reader[Db, A]

  def main(args: Array[String]): Unit = {
    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )
    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )

    val db = Db(users, passwords)

    println(findUsername(1).run(db))
    println(findUsername(2).run(db))
    println(findUsername(3).run(db))
    println()
    println(checkPassword("dade", "zerocool").run(db))
    println(checkPassword("kate", "acidburn").run(db))
    println(checkPassword("margo", "secret").run(db))
    println(checkPassword("margo", "secrett").run(db))
    println(checkPassword("kate", "secret").run(db))
    println()
    println(checkLogin(1, "zerocool").run(db))
    println(checkLogin(2, "acidburn").run(db))
    println(checkLogin(3, "secret").run(db))
    println(checkLogin(3, "secrett").run(db))
    println(checkLogin(4, "davinci").run(db))
  }

  final case class Db(
      usernames: Map[UserId, Username],
      passwords: Map[Username, Password]
  )

  private def findUsername(userId: UserId): DbReader[Option[Username]] = Reader(db => db.usernames.get(userId))

  private def checkPassword(username: Username, password: Password): DbReader[Boolean] = Reader(db =>
    db.passwords.exists {
      case (`username`, `password`) => true
      case _                        => false
    }
  )

  private def checkLogin(userId: UserId, password: Password): DbReader[Boolean] =
    for {
      usernameOpt <- findUsername(userId)
      result <- usernameOpt
        .map(checkPassword(_, password))
        .getOrElse(false.pure[DbReader])
    } yield result

}
