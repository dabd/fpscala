package fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.CommonSpec
import fpinscala.parallelism.Nonblocking.{Future, Par}
import fpinscala.parallelism.Nonblocking.Par._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalatest.prop.Checkers

class NonblockingSpec extends CommonSpec with Checkers {
  implicit val genExecutorService: Gen[ExecutorService] =
    for {
      nThreads <- Gen.choose(1, 10)
    } yield Executors.newFixedThreadPool(nThreads)

  implicit val arbExecutorService: Arbitrary[ExecutorService] =
    Arbitrary(genExecutorService)

  // The toString method of ThreadPoolExecutor reflects the internal state of the ExecutorService
  // so we try to use it for the Cogen
  implicit val cogenExecutorService: Cogen[ExecutorService] =
    Cogen[String].contramap(_.toString)

  implicit def genFuture[A: Arbitrary]: Gen[Future[A]] =
    for {
      a <- implicitly[Arbitrary[A]].arbitrary
    } yield
      new Future[A] {
        override private[parallelism] def apply(k: A => Unit) = k(a)
      }

  implicit def arbFuture[A: Arbitrary]: Arbitrary[Future[A]] =
    Arbitrary(genFuture[A])

  implicit def arbPar[A: Arbitrary]: Arbitrary[Par[A]] =
    Arbitrary {
      for {
        a <- implicitly[Arbitrary[A]].arbitrary
        f <- Arbitrary.arbFunction1[A => Unit, Unit].arbitrary
        par <- Gen.oneOf(unit(a), delay(a), lazyUnit(a), async(f))
      } yield par
    }

  "map2" should "be" in {
    forAll { (a: Par[Int], b: Par[Int], f: (Int, Int) => Int, es: ExecutorService) =>
      Par.run(es)(map2(a, b)(f)) mustBe f(Par.run(es)(a), Par.run(es)(b))
    }
  }

  "map" should "be" in {
    forAll { (a: Par[Int], f: Int => Int, es: ExecutorService) =>
      Par.run(es)(map(a)(f)) mustBe f(Par.run(es)(a))
    }
  }

  "map" should "have the fusion property" in {
    forAll { (pa: Par[Int], f: Int => Int, g: Int => Int, a: Int, es: ExecutorService) =>
      Par.run(es)(map(map(pa)(g))(f)) mustBe Par.run(es)(map(pa)(f compose g))
    }
  }

  "fork x" should "be x" in {
    forAll { (a: Par[Int], es: ExecutorService) =>
      Par.run(es)(fork(a)) mustBe Par.run(es)(a)
    }
  }

  "asyncF" should "be" in {
    forAll { (a: Int, f: Int => Int, es: ExecutorService) =>
      Par.run(es)(asyncF(f)(a)) mustBe f(a)
    }
  }

  "sequence" should "be" in {
    forAll(Gen.listOfN(20, arbPar[Int].arbitrary),
           Gen.listOfN(20, arbPar[Int].arbitrary),
           genExecutorService) {
      case (xs, ys, es: ExecutorService) =>
        Par.run(es)(sequence(xs ++ ys)) mustBe Par.run(es)(
          map2(sequence(xs), sequence(ys))(_ ++ _))
    }
  }

  "parMap" should "be" in {
    forAll(Gen.listOf(Arbitrary.arbInt.arbitrary),
           Arbitrary.arbFunction1[Int, Int].arbitrary,
           genExecutorService) {
      case (xs, f, es) =>
        Par.run(es)(parMap(xs)(f)) mustBe xs.map(f)
    }
  }

  // ex 7.11
  "choiceN" should "be" in {
    val g: Gen[(Par[Int], List[Par[Int]], ExecutorService)] =
      for {
        choices <- Gen.listOf(arbPar[Int].arbitrary) suchThat (_.nonEmpty)
        n <- Gen.choose(0, choices.length - 1)
        es <- genExecutorService
      } yield (unit(n), choices, es)
    forAll(g) {
      case (parN, choices, es) =>
        Par.run(es)(choiceN(parN)(choices)) mustBe Par.run(es)(
          choices(Par.run(es)(parN)))
    }
  }

  "choice" should "be" in {
    forAll {
      (cond: Par[Boolean], t: Par[Int], f: Par[Int], es: ExecutorService) =>
        Par.run(es)(choice(cond)(t, f)) mustBe
          Par.run(es)(if (Par.run(es)(cond)) t else f)
    }
  }

  // ex 7.12
  "choiceMap" should "be" in {
    val g: Gen[(Par[Int], Map[Int, Par[Int]], ExecutorService)] =
      for {
        m <- Gen.mapOf[Int, Par[Int]](
          Arbitrary.arbTuple2[Int, Par[Int]].arbitrary) suchThat (_.nonEmpty)
        k <- Gen.oneOf(m.keySet.toList)
        es <- genExecutorService
      } yield (unit(k), m, es)
    forAll(g) {
      case (k, m, es) =>
        Par.run(es)(choiceMap(k)(m)) mustBe Par.run(es)(m(Par.run(es)(k)))
    }
  }

  // ex 7.13
  "chooser" should "be" in {
    forAll { (pa: Par[Int], choices: Int => Par[Int], es: ExecutorService) =>
      Par.run(es)(chooser(pa)(choices)) mustBe
        Par.run(es)(choices(Par.run(es)(pa)))
    }
  }

  "choiceViaChooser" should "be like choice" in {
    forAll {
      (cond: Par[Boolean], t: Par[Int], f: Par[Int], es: ExecutorService) =>
        Par.run(es)(choiceViaChooser(cond)(t, f)) mustBe
          Par.run(es)(choice(cond)(t, f))
    }
  }

  "choiceNViaChooser" should "be like choiceN" in {
    val g: Gen[(Par[Int], List[Par[Int]], ExecutorService)] =
      for {
        choices <- Gen.listOf(arbPar[Int].arbitrary) suchThat (_.nonEmpty)
        n <- Gen.choose(0, choices.length - 1)
        es <- genExecutorService
      } yield (unit(n), choices, es)
    forAll(g) {
      case (parN, choices, es) =>
        Par.run(es)(choiceNViaChooser(parN)(choices)) mustBe
          Par.run(es)(choiceN(parN)(choices))
    }
  }

  "flatMap" should "be" in {
    forAll { (pa: Par[Int], f: Int => Par[Int], es: ExecutorService) =>
      Par.run(es)(flatMap(pa)(f)) mustBe
        Par.run(es)(f(Par.run(es)(pa)))
    }
  }

  "join" should "be" in {
    forAll { (ppa: Par[Par[Int]], es: ExecutorService) =>
      Par.run(es)(join(ppa)) mustBe Par.run(es)(Par.run(es)(ppa))
    }
  }

  "flatMapViaJoin" should "be like flatMap" in {
    forAll { (pa: Par[Int], f: Int => Par[Int], es: ExecutorService) =>
      Par.run(es)(flatMapViaJoin(pa)(f)) mustBe
        Par.run(es)(flatMap(pa)(f))
    }
  }

  "joinViaFlatMap" should "be like join" in {
    forAll { (ppa: Par[Par[Int]], es: ExecutorService) =>
      Par.run(es)(joinViaFlatMap(ppa)) mustBe
        Par.run(es)(join(ppa))
    }
  }

}
