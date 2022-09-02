package rewera.livingwithcats.monads.custom

import cats.syntax.flatMap._
import cats.syntax.functor._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import rewera.livingwithcats.functors.Tree
import rewera.livingwithcats.functors.Tree.{branch, leaf}
import rewera.livingwithcats.monads.custom.TreeMonad.treeMonad

class TreeMonadSpec extends AnyWordSpec with Matchers {

  private val func1: Int => Tree[Int] = x => branch(leaf(x - 1), leaf(x + 1))
  private val func2: Int => Tree[Int] = x => branch(leaf(x - 10), leaf(x + 10))

  "TreeMonad on flatMap" should {

    "return correct results for calling flatMap once" when {

      "the Tree has only a single Leaf" in {
        val tree = leaf(1)
        val expectedTree = branch(leaf(0), leaf(2))

        tree.flatMap(func1) mustBe expectedTree
      }

      "the Tree has single Branch with 2 Leaves" in {
        val tree = branch(leaf(1), leaf(2))
        val expectedTree = branch(branch(leaf(0), leaf(2)), branch(leaf(1), leaf(3)))

        tree.flatMap(func1) mustBe expectedTree
      }

      "the Tree has multiple nested Branches with Leaves" in {
        val tree = branch(
          left = branch(leaf(1), branch(leaf(2), leaf(3))),
          right = branch(
            branch(branch(leaf(4), branch(leaf(5), leaf(6))), leaf(7)),
            leaf(8)
          )
        )

        val expectedTree = branch(
          left = branch(
            branch(leaf(0), leaf(2)),
            branch(
              branch(leaf(1), leaf(3)),
              branch(leaf(2), leaf(4))
            )
          ),
          right = branch(
            branch(
              branch(branch(leaf(3), leaf(5)), branch(branch(leaf(4), leaf(6)), branch(leaf(5), leaf(7)))),
              branch(leaf(6), leaf(8))
            ),
            branch(leaf(7), leaf(9))
          )
        )

        tree.flatMap(func1) mustBe expectedTree
      }
    }

    "allow using for comprehension" when {

      "the Tree has only a single Leaf" in {
        val tree = leaf(1)
        val expectedTree = branch(
          branch(leaf(-10), leaf(10)),
          branch(leaf(-8), leaf(12))
        )

        val result = for {
          a <- tree
          b <- func1(a)
          c <- func2(b)
        } yield c

        result mustBe expectedTree
      }

      "the Tree has single Branch with 2 Leaves" in {
        val tree = branch(leaf(1), leaf(2))
        val expectedTree = branch(
          branch(
            branch(leaf(-10), leaf(10)),
            branch(leaf(-8), leaf(12))
          ),
          branch(
            branch(leaf(-9), leaf(11)),
            branch(leaf(-7), leaf(13))
          )
        )

        val result = for {
          a <- tree
          b <- func1(a)
          c <- func2(b)
        } yield c

        result mustBe expectedTree
      }

      "the Tree has multiple nested Branches with Leaves" in {
        val tree = branch(
          left = branch(leaf(1), branch(leaf(2), leaf(3))),
          right = branch(
            branch(branch(leaf(4), branch(leaf(5), leaf(6))), leaf(7)),
            leaf(8)
          )
        )

        val expectedTree = branch(
          left = branch(
            branch(branch(leaf(-10), leaf(10)), branch(leaf(-8), leaf(12))),
            branch(
              branch(branch(leaf(-9), leaf(11)), branch(leaf(-7), leaf(13))),
              branch(branch(leaf(-8), leaf(12)), branch(leaf(-6), leaf(14)))
            )
          ),
          right = branch(
            branch(
              branch(
                branch(branch(leaf(-7), leaf(13)), branch(leaf(-5), leaf(15))),
                branch(
                  branch(branch(leaf(-6), leaf(14)), branch(leaf(-4), leaf(16))),
                  branch(branch(leaf(-5), leaf(15)), branch(leaf(-3), leaf(17)))
                )
              ),
              branch(branch(leaf(-4), leaf(16)), branch(leaf(-2), leaf(18)))
            ),
            branch(branch(leaf(-3), leaf(17)), branch(leaf(-1), leaf(19)))
          )
        )

        val result = for {
          a <- tree
          b <- func1(a)
          c <- func2(b)
        } yield c

        result mustBe expectedTree
      }
    }
  }

}
