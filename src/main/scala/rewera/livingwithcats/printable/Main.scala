package rewera.livingwithcats.printable

import PrintableInstances._
import PrintableSyntax.PrintableOps

import cats._
import cats.implicits._

object Main extends App {

  val cat = Cat("Shiro", 7, "marble-white")

  Printable.print(cat.name)
  Printable.print(cat.age)
  Printable.print(cat.color)

  Printable.print(cat)

  println(cat.format)
  cat.print

  val showInt = Show[Int]
  println(showInt.show(123))
  println(123.show)

  println(cat.show)
}
