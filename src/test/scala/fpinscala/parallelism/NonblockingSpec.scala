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

}
