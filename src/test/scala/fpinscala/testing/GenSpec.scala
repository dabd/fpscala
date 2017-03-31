package fpinscala.testing

import fpinscala.CommonSpec
import fpinscala.state.RNG
import fpinscala.state.StateSpec._
import fpinscala.testing.Gen._
import org.scalacheck
import org.scalacheck.{Arbitrary, Shrink}
import org.scalatest.prop.Checkers

class GenSpec extends CommonSpec with Checkers {

  case class ChooseInput(start: Int, end: Int, rng: RNG)

  implicit val arbChooseInput: Arbitrary[ChooseInput] =
    Arbitrary {
      for {
        start <- scalacheck.Gen.choose(0, 100)
        stopExclusive <- scalacheck.Gen.choose(start + 1, start + 1 + 100)
        rng <- genRNG
      } yield ChooseInput(start, stopExclusive, rng)
    }

  implicit def shrinkChooseInput(implicit s: Shrink[Int],
                                 e: Shrink[Int],
                                 r: Shrink[RNG]): Shrink[ChooseInput] =
    scalacheck.Shrink {
      case ChooseInput(s, e, r) =>
        scalacheck.Shrink.shrink(s).map(ChooseInput(_, e, r)) append
          scalacheck.Shrink
            .shrink(e)
            .map(ChooseInput(s, _, r))
            .filter { case ChooseInput(s, e, _) => e > s }
    }

  // useful trick to disable shrinking in scalatest https://github.com/scalatest/scalatest/issues/584#issuecomment-104003728
//  def noShrink[T] = Shrink[T](_ => Stream.empty)
//  implicit val noShrinkChooseInput = noShrink[ChooseInput]

  implicit def arbGen[T: Arbitrary]: Arbitrary[Gen[T]] =
    Arbitrary {
      for (sample <- arbStateRNG[T].arbitrary) yield Gen(sample)
    }

  // FIXME test for shrinker
  "shrinkChooseInput" should "be" ignore {
    forAll { (c: ChooseInput) =>
      scalacheck.Shrink.shrink(c).foreach {
        println(_)
      }
    }
  }

  // ex 8.4
  "choose" should "be" in {
    forAll(arbChooseInput.arbitrary) {
      case ChooseInput(start, stopExclusive, rng) =>
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

  // ex 8.6
  "flatMap" should "be associative" in {
    forAll { (gen: Gen[Int], f: Int => Gen[Int], g: Int => Gen[Int], rng: RNG) =>
      gen.flatMap(f).flatMap(g).sample.run(rng)._1 mustBe
        gen.flatMap(f(_).flatMap(g)).sample.run(rng)._1
    }
  }

  "listOfN(unit(n))" should "be a list of n generators" in {
    forAll(for {
      gen <- arbGen[Int].arbitrary
      size <- scalacheck.Gen.choose(0, 10)
      rng <- arbRNG.arbitrary
    } yield (gen, size, rng)) {
      case (gen, size, rng) =>
        gen.listOfN(unit(size)).sample.run(rng)._1.size mustBe size
    }
  }

  // ex 8.7
  "union" should "be" in {
    forAll { (rng: RNG) =>
      Range(0, 20) must contain(
        union(choose(0, 10), choose(11, 20)).sample.run(rng)._1)
    }
  }

  // ex 8.8
  "weighted" should "be" in {
    forAll { (rng: RNG) =>
      val xs = (1 to 10000)
        .foldLeft((List(), rng): (List[Int], RNG)) {
          case ((ns, prevRng), _) =>
            val (n, currRng) =
              weighted((unit(1), 0.7), (unit(2), 0.3)).sample.run(prevRng)
            (n :: ns, currRng)
        }
        ._1

      val freqs = xs.groupBy(identity).mapValues(_.size)
      val normalizedFreqs = freqs.mapValues(_ / freqs.values.toList.sum.toDouble)

      val eps = 0.05
      normalizedFreqs(1) mustBe (0.7 +- eps)
      normalizedFreqs(2) mustBe (0.3 +- eps)
    }
  }

}
