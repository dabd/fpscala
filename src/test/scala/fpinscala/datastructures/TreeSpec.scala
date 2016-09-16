package fpinscala.datastructures

import fpinscala.CommonSpec
import org.scalacheck.{Arbitrary, Gen}

class TreeSpec extends CommonSpec {

  implicit def arbTree[T: Arbitrary]: Arbitrary[Tree[T]] = Arbitrary {
    val genLeaf = for (e <- Arbitrary.arbitrary[T]) yield Leaf(e)

    def genBranch(sz: Int): Gen[Tree[T]] =
      for {
        l <- sizedTree(sz / 2)
        r <- sizedTree(sz / 2)
      } yield Branch(l, r)

    def sizedTree(sz: Int) =
      if (sz <= 0) genLeaf
      else Gen.frequency((1, genLeaf), (3, genBranch(sz)))

    Gen.sized(sz => sizedTree(sz))
  }

  "size(t + u)" should "be size(t) + size(u)" in forAll {
    (t: Tree[Int], u: Tree[Int]) =>
      Tree.size(t + u) mustBe Tree.size(t) + Tree.size(u) + 1
  }

  "maximum(t + u)" should "be max(maximum(t) + maximum(u))" in forAll {
    (t: Tree[Int], u: Tree[Int]) =>
      Tree.maximum(t + u) mustBe Tree.maximum(t).max(Tree.maximum(u))
  }

  // https://en.wikipedia.org/wiki/Binary_tree#Depth-first_order
  "depth" should "be related to the height of a full binary tree" in forAll {
    t: Tree[Int] =>
      val n = Tree.size(t)
      val h = Tree.depth(t)

      // http://math.stackexchange.com/questions/220411/minimum-number-of-nodes-for-full-binary-tree-with-level-lambda
      n must be >= 2 * h + 1
      n.toDouble must be <= math.pow(2, h + 1) - 1
  }

  "map(t + u)(f)" should "be equal to map(t)(f) + map(u)(f)" in forAll {
    (t: Tree[Int], u: Tree[Int], f: Int => Int) =>
      Tree.map(t + u)(f) mustBe Tree.map(t)(f) + Tree.map(u)(f)
  }

  "fold(t + u)(f)(g)" should "be equal to g(fold(left(t)(f)(g), fold(right(t + u))(f)(g))" in forAll {
    (t: Tree[Int], u: Tree[Int], f: Int => Int, g: (Int, Int) => Int) =>
      t match {
        case Leaf(_) => true
        case _ =>
          Tree.fold(t + u)(f)(g) mustBe g(Tree.fold(Tree.left(t))(f)(g),
                                          Tree.fold(Tree.right(t + u))(f)(g))
      }
  }

  "sizeUsingFold" should "be size" in forAll { t: Tree[Int] =>
    Tree.sizeUsingFold(t) mustBe Tree.size(t)
  }

  "maximumUsingFold" should "be maximum" in forAll { t: Tree[Int] =>
    Tree.maximumUsingFold(t) mustBe Tree.maximum(t)
  }

  "depthUsingFold" should "be depth" in forAll { t: Tree[Int] =>
    Tree.depthUsingFold(t) mustBe Tree.depth(t)
  }

  "mapUsingFold" should "be map" in forAll { (t: Tree[Int], f: Int => Int) =>
    Tree.mapUsingFold(t)(f) mustBe Tree.map(t)(f)
  }

}
