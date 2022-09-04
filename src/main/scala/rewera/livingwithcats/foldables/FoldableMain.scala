package rewera.livingwithcats.foldables

object FoldableMain {

  def main(args: Array[String]): Unit = {
    foldingLists()
    scaffoldingOtherMethods()
  }

  private def foldingLists(): Unit = {
    println("\n--- foldingLists ---")
    val list = List(1, 2, 3, 4, 5)

    println(list.foldLeft(List.empty[Int])((acc, elem) => acc :+ elem))
    println(list.foldRight(List.empty[Int])((elem, acc) => acc :+ elem))
  }

  private def scaffoldingOtherMethods(): Unit = {
    println("\n--- scaffoldingOtherMethods ---")
    val list = List(1, 2, 3, 4, 5)

    println(list.map(_ * 2))
    println(list.mapS(_ * 2))
    println(list.flatMap(num => List(num, num * 2)))
    println(list.flatMapS(num => List(num, num * 2)))
    println(list.filter(_ % 2 != 0))
    println(list.filterS(_ % 2 != 0))
    println(list.sum)
    println(list.sumS)
  }

  implicit private class ScaffoldedList[A](list: List[A]) {

    def mapS[B](func: A => B): List[B] = list.foldLeft(List.empty[B])(_ :+ func(_))

    def flatMapS[B](func: A => IterableOnce[B]): List[B] = list.foldLeft(List.empty[B])(_ :++ func(_))

    def filterS(func: A => Boolean): List[A] =
      list.foldLeft(List.empty[A])((acc, elem) => if (func(elem)) acc :+ elem else acc)

    def sumS[B >: A](implicit num: Numeric[B]): B = list.foldLeft(num.zero)(num.plus(_, _))
  }

}
