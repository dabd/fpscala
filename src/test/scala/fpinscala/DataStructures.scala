package fpinscala

import org.scalacheck._
import DataStructures._
import DataStructures.List._

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

  property("length") = forAll { l: List[Int] =>
    l match {
      case Nil => true
      case Cons(_, t) => length(l) == length(t) + 1
    }
  }

  val genListAndSize = for {
    l <- arbList.arbitrary
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

}
