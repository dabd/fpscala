package fpinscala.testing

import fpinscala.CommonSpec
import fpinscala.state.RNG
import fpinscala.state.StateSpec._
import fpinscala.testing.Gen._
import org.scalacheck
import org.scalacheck.{Arbitrary, Shrink}
import org.scalatest.prop.Checkers

class GenSpec extends CommonSpec with Checkers {

  case class ChooseInputGen(start: Int, end: Int, rng: RNG)

  def noShrink[T] = Shrink[T](_ => Stream.empty)

  implicit val chooseGenShrink = noShrink[ChooseInputGen]

  implicit def arbGen[T: Arbitrary]: Arbitrary[Gen[T]] =
    Arbitrary {
      for (sample <- arbStateRNG[T].arbitrary) yield Gen(sample)
    }

  // ex 8.4
  "choose" should "be" in {
    forAll(for {
      start <- scalacheck.Gen.choose(0, 5)
      stopExclusive <- scalacheck.Gen.choose(start + 1, start + 1 + 5)
      rng <- genRNG
    } yield ChooseInputGen(start, stopExclusive, rng)) {
      case ChooseInputGen(start, stopExclusive, rng) =>
        choose(start, stopExclusive).sample.run(rng)._1 must
          (be >= start and be < stopExclusive)
    }
  }

  // ex 8.5
  "unit" should "be" in {
    forAll { (a: Int, rng: RNG) =>
      unit(a).sample.run(rng)._1 mustBe a
    }
  }

  "boolean" should "be" in {
    forAll { (rng: RNG) =>
      boolean.sample.run(rng)._1 must (be(false) or be(true))
    }
  }

  "listOfN" should "be" in {
    forAll(scalacheck.Gen.choose(0, 10),
           arbGen[Int].arbitrary,
           arbRNG.arbitrary) {
      case (n: Int, g: Gen[Int], rng: RNG) =>
        listOfN(n, g).sample.run(rng)._1.size mustBe n

    }
  }

}
