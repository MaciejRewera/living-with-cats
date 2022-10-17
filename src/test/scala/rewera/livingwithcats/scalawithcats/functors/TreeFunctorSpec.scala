package rewera.livingwithcats.scalawithcats.functors

import cats.implicits.toFunctorOps
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import rewera.livingwithcats.scalawithcats.models.Tree._

class TreeFunctorSpec extends AnyWordSpec with Matchers {

  private val func = (i: Int) => i.toString

  "TreeFunctor on map" when {

    "the Tree has only single Leaf" should {

      "apply given function to this single Leaf" in {
        val tree = Leaf(1)

        tree.map(func) mustBe Leaf("1")
      }
    }

    "the Tree has single Branch with 2 Leaves" should {

      "apply given function to all Leaves" in {
        val tree = Branch(Leaf(1), Leaf(2))
        val expectedTree = Branch(Leaf("1"), Leaf("2"))

        tree.map(func) mustBe expectedTree
      }
    }

    "the Tree has multiple nested Branches with Leaves" should {

      "apply given function to all Leaves" in {
        val tree = Branch(
          left = Branch(Leaf(1), Branch(Leaf(2), Leaf(3))),
          right = Branch(
            Branch(Branch(Leaf(4), Branch(Leaf(5), Leaf(6))), Leaf(7)),
            Leaf(8)
          )
        )
        val expectedTree = Branch(
          left = Branch(Leaf("1"), Branch(Leaf("2"), Leaf("3"))),
          right = Branch(
            Branch(Branch(Leaf("4"), Branch(Leaf("5"), Leaf("6"))), Leaf("7")),
            Leaf("8")
          )
        )

        tree.map(func) mustBe expectedTree
      }
    }
  }
}
