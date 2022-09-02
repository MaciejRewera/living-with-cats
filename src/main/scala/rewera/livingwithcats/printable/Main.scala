package rewera.livingwithcats.printable

import cats._
import cats.implicits._
import rewera.livingwithcats.models.{Box, Cat}
import rewera.livingwithcats.printable.PrintableInstances._
import rewera.livingwithcats.printable.PrintableSyntax.PrintableOps

object Main {

  def main(args: Array[String]): Unit = {
    catsMain()
    contramapMain()
  }

  def catsMain(): Unit = {
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

    val cat1 = Cat("Garfield", 38, "orange and black")
    val cat2 = Cat("Heathcliff", 33, "orange and black")
    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

    println("-------------")
    println(cat1 === cat2)
    println(cat2 === cat1)
    println(cat1 === cat1)
    println(cat2 === cat2)
    println("-------------")
    println(optionCat1 === optionCat2)
    println(optionCat2 === optionCat1)
    println(optionCat1 === optionCat1)
    println(optionCat2 === optionCat2)
  }

  def contramapMain(): Unit = {
    val box1 = Box("Hello World!")
    val box2 = Box(true)

    println(Printable.format(box1))
    println(Printable.format(box2))
  }
}
