package rewera.livingwithcats.printable

object PrintableSyntax {

  implicit class PrintableOps[A](value: A) {

    def format(implicit printable: Printable[A]): String = Printable.format(value)

    def print(implicit printable: Printable[A]): Unit = Printable.print(value)
  }
}
