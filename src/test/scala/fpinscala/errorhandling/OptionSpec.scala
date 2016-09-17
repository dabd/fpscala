package fpinscala.errorhandling

import fpinscala.CommonSpec
import org.scalacheck.{Arbitrary, Gen}

class OptionSpec extends CommonSpec {

  implicit def arbOption[T: Arbitrary]: Arbitrary[Option[T]] =
    Arbitrary {
      val genNone = Gen.const(None)
      val genSome = for { x <- Arbitrary.arbitrary[T] } yield Some(x)
      Gen.frequency((1, genNone), (9, genSome))
    }

  "map" should "be" in forAll { (i: Int, f: Int => Int) =>
    None.map(f) mustBe None
    Some(i).map(f) mustBe Some(f(i))
  }

  "flatMap" should "be" in forAll { (i: Int, f: Int => Option[Int]) =>
    None.flatMap(f) mustBe None
    Some(i).flatMap(f) mustBe f(i)
  }

  "getOrElse" should "be" in forAll { (i: Int, default: Int) =>
    None.getOrElse(default) mustBe default
    Some(i).getOrElse(default) mustBe i
  }

  "orElse" should "be" in forAll { (a: Int, ob: Option[Int]) =>
    None.orElse(ob) mustBe ob
    Some(a).orElse(ob) mustBe Some(a)
  }

  "filter" should "be" in forAll { (i: Int, f: Int => Boolean) =>
    None.filter(f) mustBe None
    Some(i).filter(_ == i) mustBe Some(i)
    Some(i).filter(_ != i) mustBe None
  }

  "variance" should "be equal to variance2" in {
    import org.scalactic.TolerantNumerics

    val epsilon = 1e-2d
    implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

    forAll(Gen.listOfN(50, Gen.choose(-10.0, 10.0))) { xs: List[Double] =>
      Option.variance(xs) === Option.variance2(xs)
    }
  }

  "map2" should "be" in forAll { (a: Int, b: Int, f: (Int, Int) => Int) =>
    Option.map2(None, Some(a))(f) mustBe None
    Option.map2(Some(a), None)(f) mustBe None
    Option.map2(Some(a), Some(b))(f) mustBe Some(f(a, b))
  }

  "map2_2" should "be equal to map2" in forAll {
    (a: Option[Int], b: Option[Int], f: (Int, Int) => Int) =>
      Option.map2_2(a, b)(f) mustBe Option.map2(a, b)(f)
  }

  "sequence" should "be" in forAll { xs: List[Option[Int]] =>
    Option.sequence(xs) mustBe (if (!xs.contains(None))
                                  Some(xs.map(_.getOrElse(None)))
                                else None)
  }

  "traverse" should "be" in forAll { (xs: List[Int], f: Int => Option[Int]) =>
    Option.traverse(xs)(f) mustBe Option.sequence(xs.map(f))
  }
}
