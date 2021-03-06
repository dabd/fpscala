package fpinscala.datastructures

import fpinscala.CommonSpec
import fpinscala.datastructures.List._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.{apply => _}
import org.scalacheck.{Arbitrary, Gen}

class ListSpec extends CommonSpec {

  import Arbitrary.arbFunction1

  implicit def arbList[T: Arbitrary]: Arbitrary[List[T]] = Arbitrary {
    val genNil = Gen.const(Nil)

    def genCons(sz: Int): Gen[List[T]] =
      for {
        x <- arbitrary[T]
        xs <- sizedList(sz / 2)
      } yield Cons(x, xs)

    def sizedList(sz: Int) =
      if (sz <= 0) genNil
      else Gen.frequency((1, genNil), (3, genCons(sz)))

    Gen.sized(sz => sizedList(sz))
  }

  "tail" should "be t" in forAll { l: List[Int] =>
    l match {
      case Nil => true
      case Cons(_, t) => tail(l) mustBe t
    }
  }

  "setHead(Cons(_, t))" should "be Cons(h, t)" in forAll {
    (l: List[Int], h: Int) =>
      l match {
        case Nil => true
        case Cons(_, t) => setHead(l, h) mustBe Cons(h, t)
      }
  }

  "length(xs ++ ys)" should "be length(xs) + length(ys)" in forAll {
    (xs: List[Int], ys: List[Int]) =>
      List.length(append(xs, ys)) mustBe List.length(xs) + List.length(ys)
  }

  "drop(xs ++ ys, length(xs))" should "be ys" in forAll {
    (xs: List[Int], ys: List[Int]) =>
      drop(append(xs, ys), List.length(xs)) mustBe ys
  }

  "appending takeWhile and dropWhile" should "be the original list" in forAll {
    (l: List[Int], f: Int => Boolean) =>
      l mustBe append(takeWhile(l, f), dropWhile(l, f))
  }

  "takeWhile" should "be" in forAll { (l: List[Int], f: Int => Boolean) =>
    toScalaList(takeWhile(l, f)) mustBe toScalaList(l).takeWhile(f)
  }

  "dropWhile" should "be" in forAll { (l: List[Int], f: Int => Boolean) =>
    toScalaList(dropWhile(l, f)) mustBe toScalaList(l).dropWhile(f)
  }

  "init(t :: [h])" should "be t" in forAll { (l: List[Int]) =>
    l match {
      case Nil => true
      case Cons(h, t) => init(append(t, Cons(h, Nil))) mustBe t
    }
  }

  "reverse(reverse(l)" should "be l" in forAll { l: List[Int] =>
    reverse(reverse(l)) mustBe l
  }

  "reverse" should "be distributive in relation to append" in forAll {
    (xs: List[Int], ys: List[Int]) =>
      reverse(append(xs, ys)) mustBe append(reverse(ys), reverse(xs))
  }

  "append" should "be associative" in forAll {
    (xs: List[Int], ys: List[Int], zs: List[Int]) =>
      append(append(xs, ys), zs) mustBe append(xs, append(ys, zs))
  }

  "length2" should "be equal to length" in forAll { l: List[Int] =>
    length2(l) mustBe List.length(l)
  }

  // foldRight universal property of fold http://www.cs.nott.ac.uk/~pszgmh/fold.pdf
  "foldRight" should "satisfy the universal property" in forAll {
    (l: List[Int], f: (Int, Int) => Int, v: Int) =>
      def g1(l: List[Int]): Int = l match {
        case Nil => v
        case Cons(h, t) => f(h, g1(t))
      }

      def g2(l: List[Int]): Int = foldRight(l, v)(f)

      val p1 = g1(l) == foldRight(l, v)(f)

      val p2 = l match {
        case Nil => g2(l) == v
        case Cons(h, t) => g2(l) == f(h, g2(t))
      }

      p1 && p2 mustBe true
  }

  "foldRight(xs ++ ys, v)(f)" should "be foldRight(xs, foldRight(ys, v)(f))(f)" in forAll {
    (xs: List[Int], ys: List[Int], v: Int, f: (Int, Int) => Int) =>
      foldRight(append(xs, ys), v)(f) mustBe foldRight(xs,
                                                       foldRight(ys, v)(f))(f)
  }

  // since foldLeft can be defined in terms of foldRight by foldl f v xs = fold (λx g → (λa → g (f a x))) id xs v
  "foldLeft" can "be defined in terms of foldRight" in forAll {
    (l: List[Int], f: (Int, Int) => Int, v: Int) =>
      foldLeft(l, v)(f) mustBe foldRight(l, (b: Int) => b)((a, g) =>
        b => g(f(b, a)))(v)
  }

  "sum" should "equal immutable.List.sum" in forAll { l: List[Int] =>
    sum(l) mustBe toScalaList(l).sum
  }

  "product" should "equal immutable.List.product" in forAll { l: List[Int] =>
    product(l) mustBe toScalaList(l).product
  }

  "length3" should "be equal to length" in forAll { l: List[Int] =>
    length3(l) mustBe List.length(l)
  }

  "reverse2" should "be equal to reverse" in forAll { l: List[Int] =>
    reverse2(l) mustBe reverse(l)
  }

  "foldLeftInTermsOfFoldRight" should "be equal to foldLeft" in forAll {
    (l: List[Int], f: (Int, Int) => Int, v: Int) =>
      foldLeftInTermsOfFoldRight(l, v)(f) mustBe foldLeft(l, v)(f)
  }

  "foldRightInTermsOfFoldLeft" should "be equal to foldRight" in forAll {
    (l: List[Int], f: (Int, Int) => Int, v: Int) =>
      foldRightInTermsOfFoldLeft(l, v)(f) mustBe foldRight(l, v)(f)
  }

  "append2" should "be equal to append" in forAll {
    (xs: List[Int], ys: List[Int]) =>
      append2(xs, ys) mustBe append(xs, ys)
  }

  "flatten using fold" should "be equal to recursive flatten" in forAll {
    l: List[List[Int]] =>
      def recFlatten(l: List[List[Int]]): List[Int] = l match {
        case Nil => Nil
        case Cons(h, t) => append(h, recFlatten(t))
      }
      flatten(l) mustBe recFlatten(l)
  }

  "add1 using fold" should "be equal to recursive add1" in forAll {
    l: List[Int] =>
      add1(l) mustBe add1Recursive(l)
  }

  "listDoubleToString using fold" should "be equal to recursive listDoubleToString" in forAll {
    l: List[Double] =>
      def recListDoubleToString(l: List[Double]): List[String] = l match {
        case Nil => Nil
        case Cons(h, t) => Cons(h.toString, recListDoubleToString(t))
      }
      listDoubleToString(l) mustBe recListDoubleToString(l)
  }

  "map using fold" should "be equal to recursive map" in forAll {
    (l: List[Int], f: Int => Int) =>
      map(l, f) mustBe mapRecursive(l, f)
  }

  "filter using fold" should "be equal to recursive filter" in forAll {
    (l: List[Int], f: Int => Boolean) =>
      filter(l)(f) mustBe filterRecursive(l)(f)
  }

  "flatMap using fold" should "be equal to recursive flatMap" in forAll {
    (l: List[Int], f: Int => List[Int]) =>
      flatMap(l)(f) mustBe flatMapRecursive(l)(f)
  }

  "filter2 using flatMap" should "be equal to recursive filter" in forAll {
    (l: List[Int], f: Int => Boolean) =>
      filter2(l)(f) mustBe filterRecursive(l)(f)
  }

  "addLists using zipWith" should "be equal to recursive addLists" in forAll {
    (xs: List[Int], ys: List[Int]) =>
      addLists(xs, ys) mustBe addListsRecursive(xs, ys)
  }

  "zipWith using fold" should "be equal to recursive zipWith" in forAll {
    (xs: List[Int], ys: List[Int], f: (Int, Int) => Int) =>
      zipWith(xs, ys, f) mustBe zipWithRecursive(xs, ys, f)
  }

  "startsWith" should "be" in forAll { (xs: List[Int], ys: List[Int]) =>
    val xys = append(xs, ys)
    startsWith(xys, xs) mustBe true
    if (ys != Nil)
      startsWith(xs, xys) mustBe false
  }

  "exists" should "be" in forAll { (xs: List[Int], ys: List[Int]) =>
    val xys = append(xs, ys)
    List.existsTail(xys, (_: List[Int]) == ys) mustBe
      (if (xys == Nil || ys == Nil) false
       else true)
  }

  "hasSubsequence" should "be" in forAll { (xs: List[Int], ys: List[Int]) =>
    val xys = append(xs, ys)
    hasSubsequence(xys, xs) mustBe true
    hasSubsequence(xys, ys) mustBe true
    if (ys != Nil)
      hasSubsequence(xs, xys) mustBe false
  }

}
