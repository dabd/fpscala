package fpinscala

import org.scalacheck._
import DataStructures._
import DataStructures.List._
import com.sun.javafx.geom.transform.Identity

object DataStructuresSpec extends Properties("DataStructuresSpec") {

  import org.scalacheck.Prop.forAll

  implicit val arbList: Arbitrary[List[Int]] = Arbitrary {

    val genNil = Gen.const(Nil)

    lazy val genCons = for {
      h <- Arbitrary.arbitrary[Int]
      t <- genList
    } yield Cons(h, t)

    def genList: Gen[List[Int]] = Gen.oneOf(genNil, genCons)

    genList
  }

  property("tail") = forAll { l: List[Int] =>
    l match {
      case Nil => true
      case Cons(_, t) => tail(l) == t
    }
  }

  property("setHead") = forAll { (l: List[Int], h: Int) =>
    l match {
      case Nil => true
      case Cons(_, t) => setHead(l, h) == Cons(h, t)
    }
  }

  property("length2") = forAll { l: List[Int] =>
    l match {
      case Nil => length2(l) == 0
      case Cons(_, t) => length2(l) == length2(t) + 1
    }
  }

  val genListAndSize = for {
    l <- arbList.arbitrary
    n <- Gen.choose(0, length2(l))
  } yield (l, n)

  property("drop") = forAll(genListAndSize) {
    case (l, n) =>
      (l, n) match {
        case (Nil, _) => drop(l, n) == Nil
        case (Cons(_, t), 0) => drop(l, n) == l
        case (Cons(_, t), n) => drop(l, n) == drop(t, n - 1)
      }
  }

  import Arbitrary.arbFunction1

  property("dropWhile") = forAll { (l: List[Int], f: Int => Boolean) =>
    l match {
      case Nil => dropWhile(Nil, f) == Nil
      case Cons(h, t) if f(h) => dropWhile(l, f) == dropWhile(t, f)
      case Cons(h, t) if !f(h) => dropWhile(l, f) == l
    }
  }

  property("init") = forAll { l: List[Int] =>
    l match {
      case Nil => true
      case Cons(h, t) => init(l) == reverse(tail(reverse(l)))
    }
  }

  property("reverse") = forAll { l: List[Int] =>
    reverse(reverse(l)) == l
  }

  property("append") = forAll { (xs: List[Int], ys: List[Int]) =>
    append(xs, ys) == reverse(append(reverse(ys), reverse(xs)))
  }

  property("length") = forAll { l: List[Int] =>
    l match {
      case Nil => length(l) == 0
      case Cons(_, t) => length(l) == length(t) + 1
    }
  }

  // foldRight universal property of fold (left to right http://www.cs.nott.ac.uk/~pszgmh/fold.pdf)
  val foldRightUniversal1 =
    forAll { (l: List[Int], f: (Int, Int) => Int, v: Int) =>
      def g(l2: List[Int]): Int = l2 match {
        case Nil => v
        case Cons(h, t) => f(h, (g(t)))
      }

      g(l) == foldRight(l, v)(f)
    }

  // foldRight universal property of fold (right to left http://www.cs.nott.ac.uk/~pszgmh/fold.pdf)
  val foldRightUniversal2 =
    forAll { (l: List[Int], f: (Int, Int) => Int, v: Int) =>
      def g(l2: List[Int]): Int = foldRight(l2, v)(f)

      l match {
        case Nil => g(l) == v
        case Cons(h, t) => g(l) == f(h, g(t))
      }

      g(l) == foldRight(l, v)(f)
    }

  property("foldRight universal property") = forAll {
    (l: List[Int], f: (Int, Int) => Int, v: Int) =>
      foldRightUniversal1 && foldRightUniversal2
  }

  // since foldLeft can be defined in terms of foldRight by foldl f v xs = fold (λx g → (λa → g (f a x))) id xs v
  property("foldLeft") = forAll {
    (l: List[Int], f: (Int, Int) => Int, v: Int) =>
      foldLeft(l, v)(f) == foldRight(l, (b: Int) => b) ((a, g) => b => g(f(b,a)))(v)
  }

}
