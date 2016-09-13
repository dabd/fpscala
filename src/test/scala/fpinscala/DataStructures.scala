package fpinscala

import fpinscala.DataStructures.List._
import fpinscala.DataStructures._
import org.scalacheck._

object DataStructuresSpec extends Properties("DataStructuresSpec") {

  import org.scalacheck.Prop.forAll

  def arbListT[T](gen: Gen[T]): Arbitrary[List[T]] = Arbitrary {
    val genNil = Gen.const(Nil)

    lazy val genCons = for {
      h <- gen
      t <- genList
    } yield Cons(h, t)

    def genList: Gen[List[T]] = Gen.oneOf(genNil, genCons)

    genList
  }

  implicit val arbListInt: Arbitrary[List[Int]] =
    arbListT[Int](Arbitrary.arbInt.arbitrary)

  implicit val arbNestedListInt: Arbitrary[List[List[Int]]] =
    arbListT[List[Int]](arbListInt.arbitrary)

  implicit val arbListDouble: Arbitrary[List[Double]] =
    arbListT[Double](Arbitrary.arbDouble.arbitrary)

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

  property("length") = forAll { l: List[Int] =>
    l match {
      case Nil => length(l) == 0
      case Cons(_, t) => length(l) == length(t) + 1
    }
  }

  val genListAndSize = for {
    l <- arbListInt.arbitrary
    n <- Gen.choose(0, length(l))
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
      case Cons(h, t) => init(l) == reverse(tail(reverse2(l)))
    }
  }

  property("reverse is its own inverse") = forAll { l: List[Int] =>
    reverse(reverse(l)) == l
  }

  property("append") = forAll { (xs: List[Int], ys: List[Int]) =>
    append(xs, ys) == reverse(append(reverse(ys), reverse(xs)))
  }

  property("length2") = forAll { l: List[Int] =>
    length2(l) == length(l)
  }

  // foldRight universal property of fold (left to right http://www.cs.nott.ac.uk/~pszgmh/fold.pdf)
  val foldRightUniversal1 =
    forAll { (l: List[Int], f: (Int, Int) => Int, v: Int) =>
      def g(l: List[Int]): Int = l match {
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
    }

  property("foldRight universal property") = forAll {
    (l: List[Int], f: (Int, Int) => Int, v: Int) =>
      foldRightUniversal1 && foldRightUniversal2
  }

  // since foldLeft can be defined in terms of foldRight by foldl f v xs = fold (λx g → (λa → g (f a x))) id xs v
  property("foldLeft") = forAll { (l: List[Int], f: (Int, Int) => Int,
                                   v: Int) =>
    foldLeft(l, v)(f) == foldRight(l, (b: Int) => b)((a, g) =>
      b => g(f(b, a)))(v)
  }

  property("length3") = forAll { l: List[Int] =>
    length3(l) == length(l)
  }

  property("reverse2") = forAll { l: List[Int] =>
    reverse2(l) == reverse(l)
  }

  property("foldLeftInTermsOfFoldRight") = forAll {
    (l: List[Int], f: (Int, Int) => Int, v: Int) =>
      foldLeftInTermsOfFoldRight(l, v)(f) == foldLeft(l, v)(f)
  }

  property("foldRightInTermsOfFoldLeft") = forAll {
    (l: List[Int], f: (Int, Int) => Int, v: Int) =>
      foldRightInTermsOfFoldLeft(l, v)(f) == foldRight(l, v)(f)
  }

  property("append2") = forAll { (xs: List[Int], ys: List[Int]) =>
    append2(xs, ys) == append(xs, ys)
  }

  property("flatten") = forAll { l: List[List[Int]] =>
    def recFlatten(l: List[List[Int]]): List[Int] = l match {
      case Nil => Nil
      case Cons(h, t) => append(h, recFlatten(t))
    }
    flatten(l) == recFlatten(l)
  }

  property("add1") = forAll { l: List[Int] =>
    add1(l) == add1Recursive(l)
  }

  property("listDoubleToString") = forAll { l: List[Double] =>
    def recListDoubleToString(l: List[Double]): List[String] = l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h.toString, recListDoubleToString(t))
    }
    listDoubleToString(l) == recListDoubleToString(l)
  }

  property("map") = forAll { (l: List[Int], f: Int => Int) =>
    map(l, f) == mapRecursive(l, f)
  }

  property("filter") = forAll { (l: List[Int], f: Int => Boolean) =>
    filter(l)(f) == filterRecursive(l)(f)
  }

  property("flatMap") = forAll { (l: List[Int], f: Int => List[Int]) =>
    flatMap(l)(f) == flatMapRecursive(l)(f)
  }

  property("filter2") = forAll { (l: List[Int], f: Int => Boolean) =>
    filter2(l)(f) == filterRecursive(l)(f)
  }

  val addListsLengthIsSmallerOfTheTwo = forAll {
    (xs: List[Int], ys: List[Int]) =>
      length(addLists(xs, ys)) == length(xs).min(length(ys))
  }

  val genListsAndSize = for {
    xs <- arbListInt.arbitrary
    ys <- arbListInt.arbitrary
    n <- Gen.choose(0, length(xs).min(length(ys)))
  } yield (xs, ys, n)

  property("addLists") = addListsLengthIsSmallerOfTheTwo && forAll(
      genListsAndSize) {
    case (xs, ys, n) =>
      (for {
        z <- nth(n, addLists(xs, ys))
        x <- nth(n, xs)
        y <- nth(n, ys)
      } yield z == x + y).getOrElse(true)
  }

  property("zipWith") = forAll { (xs: List[Int], ys: List[Int], f: (Int, Int) => Int) =>
    zipWith(xs, ys, f) == zipWithRecursive(xs, ys, f)
  }


}
