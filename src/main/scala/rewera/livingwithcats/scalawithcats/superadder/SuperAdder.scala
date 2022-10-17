package rewera.livingwithcats.scalawithcats.superadder

import cats.Monoid
import cats.implicits.catsSyntaxSemigroup

object SuperAdder extends App {

  def add[T: Monoid](items: List[T]): T = items.fold(Monoid[T].empty)(_ |+| _)

  val items = List(1, 2, 3, 4, 5, 10, 15)
  val result = add(items)
  println(result)

  val optionItems = items.map(Option(_))
  val resultOptions = add(optionItems)
  println(resultOptions)

  val stringItems = items.map(_.toString)
  val resultStrings = add(stringItems)
  println(resultStrings)

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid: Monoid[Order] =
    Monoid.instance[Order](Order(0.0, 0.0), (a, b) => Order(a.totalCost |+| b.totalCost, a.quantity |+| b.quantity))

  val order1 = Order(1.1, 2.123)
  val order2 = Order(2.2, 4.734)
  val orderResult = add(List(order1, order2))
  println(orderResult)
}
