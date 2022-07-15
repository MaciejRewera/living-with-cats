package rewera.livingwithcats.printable

import cats.Show
import cats.implicits._

case class Cat(name: String, age: Int, color: String)

object Cat {

  implicit val catShow: Show[Cat] = Show.show[Cat] { cat =>
    val name  = cat.name.show
    val age   = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }
}
