package rewera.livingwithcats.scalawithcats.monads.custom

import cats.Monad
import rewera.livingwithcats.scalawithcats.models.Tree
import rewera.livingwithcats.scalawithcats.models.Tree._

object TreeMonad {

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def flatMap[A, B](tree: Tree[A])(func: A => Tree[B]): Tree[B] = tree match {
      case Leaf(value)         => func(value)
      case Branch(left, right) => Branch(flatMap(left)(func), flatMap(right)(func))
    }

    override def tailRecM[A, B](a: A)(func: A => Tree[Either[A, B]]): Tree[B] = flatMap(func(a)) {
      case Right(value) => leaf(value)
      case Left(value)  => tailRecM(value)(func)
    }

    override def pure[A](x: A): Tree[A] = Leaf(x)
  }
}
