package rewera.livingwithcats.scalawithcats.models

import cats.Functor
import cats.implicits.toFunctorOps

sealed trait Tree[+A]

object Tree {
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val branchFunctor: Functor[Branch] = new Functor[Branch] {
    override def map[A, B](fa: Branch[A])(f: A => B): Branch[B] =
      Branch(left = Functor[Tree].map(fa.left)(f), right = Functor[Tree].map(fa.right)(f))
  }

  implicit val leafFunctor: Functor[Leaf] = new Functor[Leaf] {
    override def map[A, B](fa: Leaf[A])(f: A => B): Leaf[B] = Leaf(f(fa.value))
  }

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case leaf: Leaf[A]     => leaf.map(f)
      case branch: Branch[A] => branch.map(f)
    }
  }

}
