package fpinscala.state

import fpinscala.CommonSpec
import fpinscala.state.RNG.{Rand, Simple}
import org.scalacheck._
import org.scalacheck.commands.Commands
import org.scalatest.prop.Checkers

class StateSpec extends CommonSpec with Checkers {

  val genRNG = for {
    seed <- Arbitrary.arbLong.arbitrary
  } yield Simple(seed)

  implicit val arbitraryRNG: Arbitrary[RNG] = Arbitrary(genRNG)

  "nonNegativeInt" should "be" in forAll(genRNG, MinSuccessful(1000000)) {
    rng =>
      val (i, _) = RNG.nonNegativeInt(rng)
      i mustBe >=(0)
  }

  "nonNegativeIntSkewed" should "be" in {
    check(
      Prop.forAll(genRNG) { rng =>
        val (i, _) = RNG.nonNegativeIntSkewed(rng)
        Prop
          .classify(rng.nextInt._1 == Int.MinValue, "nextInt = Int.MinValue") {
            Prop.classify(rng.nextInt._1 == 5, "nextInt = ") {
              i >= 0
            }
          }
      },
      MinSuccessful(1000000)
    )
  }

  "double" should "be in [0, 1[" in forAll(genRNG, MinSuccessful(1000000)) {
    rng =>
      val (d, _) = RNG.double(rng)
      d mustBe >=(0.toDouble)
      d mustBe <(1.toDouble)
  }

  "ints" should "be equal to intsTailRec" in forAll(genRNG, Gen.choose(0, 50)) {
    case (rng, count) =>
      RNG.ints(count)(rng) mustBe RNG.intsTailRec(count)(rng)
  }

  // https://gitter.im/typelevel/cats?at=581253788ed1c0ff5c3619a9
  implicit val cogenRNG: Cogen[RNG] = Cogen[Int].contramap(_.nextInt._1)

  implicit def arbitraryRand[T: Arbitrary]: Arbitrary[RNG.Rand[T]] =
    Arbitrary.arbFunction1[RNG, (T, RNG)]

  "map" should "be" in forAll { (s: Rand[Int], f: Int => Int, rng: RNG) =>
    val (a, rng2) = s(rng)
    RNG.map(s)(f)(rng) mustBe (f(a), rng2)
  }

  "doubleUsingMap" should "be equal to double" in forAll(genRNG) { rng =>
    RNG.doubleUsingMap(rng) mustBe RNG.double(rng)
  }

  "map2" should "be" in forAll(arbitraryRand[Int].arbitrary,
                               arbitraryRand[Int].arbitrary,
                               Arbitrary.arbFunction2[Int, Int, Int].arbitrary,
                               genRNG) {
    case (r1, r2, f, rng) =>
      val rngResult = RNG.map2[Int, Int, Int](r1, r2)(f)
      val (i, r4) = rngResult(rng)

      val (a, ra) = r1(rng)
      val (b, rb) = r2(ra)

      i mustBe f(a, b)

      r4 mustBe rb
  }

  "sequence" should "be" in forAll {
    (fs: List[Rand[Int]], gs: List[Rand[Int]], rng: RNG) =>
      val (l, r) = RNG.sequence(fs)(rng)
      val (g, s) = RNG.sequence(gs)(r)

      RNG.sequence(fs ++ gs)(rng) mustBe (l ++ g, s)
  }

  "intsViaSequence" should "be equal to ints" in forAll(genRNG,
                                                        Gen.choose(0, 50)) {
    case (rng, count) =>
      RNG.ints(count)(rng) mustBe RNG.intsViaSequence(count)(rng)
  }

  "flatMap" should "be" in forAll { (f: Rand[Int], g: Int => Rand[Int], rng: RNG) =>
    val (a, rng2) = f(rng)
    RNG.flatMap(f)(g)(rng) mustBe g(a)(rng2)
  }

  "nonNegativeLessThan" should "be" in forAll { (n: Int, rng: RNG) =>
    whenever(n > 0) {
      val (i, _) = RNG.nonNegativeLessThan(n)(rng)
      i mustBe >=(0)
      i mustBe <(n)
    }
  } // TODO pass configuration to minSuccessful

  "mapViaFlatMap" should "be equal to map" in forAll { (s: Rand[Int], f: Int => Int, rng: RNG) =>
    RNG.mapViaFlatMap(s)(f)(rng) mustBe RNG.map(s)(f)(rng)
  }

  "map2ViaFlatMap" should "be equal to map" in forAll {
    (ra: Rand[Int], rb: Rand[Int], f: (Int, Int) => Int, rng: RNG) =>
      RNG.map2viaFlatMap(ra, rb)(f)(rng) mustBe RNG.map2(ra, rb)(f)(rng)
  }

  implicit def arbStateRNG[T: Arbitrary]: Arbitrary[State[RNG, T]] =
    Arbitrary {
      for {
        run <- Arbitrary.arbFunction1[RNG, (T, RNG)].arbitrary
      } yield State(run)
    }

  "State.flatMap" should "be" in forAll { (s: State[RNG, Int], f: Int => State[RNG, Int], rng: RNG) =>
    val (a, rng2) = s.run(rng)
    s.flatMap(f).run(rng) must (equal(f(a).run(rng2)) and
      equal(RNG.flatMap(s.run)(f(_).run)(rng)))
  }

  "State.unit" should "be" in forAll { (n: Int, rng: RNG) =>
    State.unit[RNG, Int](n).run(rng) must (equal(
      State[RNG, Int](s => (n, s)).run(rng))
      and equal(RNG.unit(n)(rng)))
  }

  "State.map" should "be" in forAll { (s: State[RNG, Int], f: Int => Int, rng: RNG) =>
    s.map(f).run(rng) mustBe RNG.map(s.run)(f)(rng)
  }

  "State.map2" should "be" in forAll {
    (s: State.Rand[Int],
     s2: State.Rand[Int],
     f: (Int, Int) => Int,
     rng: RNG) =>
      s.map2(s2)(f).run(rng) mustBe RNG.map2(s.run, s2.run)(f)(rng)
  }

  "State.sequence" should "be" in forAll {
    (fs: List[State[RNG, Int]], rng: RNG) =>
      State.sequence(fs).run(rng) mustBe RNG.sequence(fs.map(_.run))(rng)
  }

}
