package fpinscala.errorhandling

import fpinscala.CommonSpec
import org.scalacheck.{Arbitrary, Gen}

class EitherSpec extends CommonSpec {

  implicit def arbEither[E, A](implicit e: Arbitrary[E],
                               a: Arbitrary[A]): Arbitrary[Either[E, A]] =
    Arbitrary {
      val genE = for { e <- Arbitrary.arbitrary[E] } yield Left(e)
      val genA = for { a <- Arbitrary.arbitrary[A] } yield Right(a)
      Gen.frequency((1, genE), (9, genA))
    }

  "left" should "be" in forAll { x: Either[String, Int] =>
    x match {
      case Left(e) => e
      case _ => true
    }
  }

  "right" should "be" in forAll { x: Either[String, Int] =>
    x match {
      case Right(a) => a
      case _ => true
    }
  }

  "isLeft" should "be" in forAll { x: Either[String, Int] =>
    x match {
      case Left(_) => true
      case _ => false
    }
  }

  "isRight" should "be" in forAll { x: Either[String, Int] =>
    x match {
      case Right(_) => true
      case _ => false
    }
  }

  "map" should "be" in forAll { (e: String, a: Int, f: Int => Int) =>
    Left(e).map(f) mustBe Left(e)
    Right(a).map(f) mustBe Right(f(a))
  }

  "flatMap" should "be" in forAll { (e: String, a: Int, f: Int => Either[
                                       String,
                                       Int]) =>
    Left(e).flatMap(f) mustBe Left(e)
    Right(a).flatMap(f) mustBe f(a)
  }

  "orElse" should "be" in forAll {
    (e: String, a: Int, b: Either[String, Int]) =>
      Left(e).orElse(b) mustBe b
      Right(a).orElse(b) mustBe Right(a)
  }

  "map2" should "be" in forAll {
    (e1: String, a1: Int, e2: String, a2: Int, f: (Int, Int) => Int) =>
      Left(e1).map2(Right(a2))(f) mustBe Left(e1)
      Right(a1).map2(Left(e2))(f) mustBe Left(e2)
      Right(a1).map2(Right(a2))(f) mustBe Right(f(a1, a2))
  }

  "map2_2" should "be equal to map2" in {
    (a: Either[String, Int], b: Either[String, Int], f: (Int, Int) => Int) =>
      a.map2_2(b)(f) mustBe a.map2(b)(f)
  }

  "sequence" should "be" in forAll { xs: List[Either[String, Int]] =>
    Either.sequence(xs) mustBe (if (!xs.exists(_.isLeft))
                                  Right(xs.map(_.right))
                                else xs.filter(_.isLeft).head)
  }

  "sequence2" should "be equal to sequence" in forAll {
    xs: List[Either[String, Int]] =>
      Either.sequence2(xs) mustBe Either.sequence(xs)
  }

  "traverse" should "be" in forAll { (xs: List[Int], f: Int => Either[String,
                                                                      Int]) =>
    Either.traverse(xs)(f) mustBe Either.sequence(xs.map(f))
  }
}
