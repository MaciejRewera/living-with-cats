package rewera.livingwithcats.printable


object PrintableInstances {

  implicit val stringPrintable: Printable[String] = (value: String) => value

  implicit val intPrintable: Printable[Int] = (value: Int) => String.valueOf(value)

  implicit val catPrintable: Printable[Cat] = (cat: Cat) => {
    val name = Printable.format(cat.name)
    val age = Printable.format(cat.age)
    val color = Printable.format(cat.color)
    s"$name is a $age year-old $color cat."
  }

  implicit def optionPrintable[A](implicit printable: Printable[A]): Printable[Option[A]] = {
    case Some(value) => printable.format(value)
    case None => ""
  }
}
