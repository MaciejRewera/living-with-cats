package rewera.livingwithcats.scalawithcats.monads

import cats.Id

trait IdOps {
  def pure[A](value: A): Id[A] = value

  def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)

  def map[A, B](value: Id[A])(func: A => B): Id[B] = func(value)
}
