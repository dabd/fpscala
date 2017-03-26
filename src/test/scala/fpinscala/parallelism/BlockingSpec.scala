package fpinscala.parallelism

import java.util.concurrent._

import fpinscala.CommonSpec
import fpinscala.parallelism.Blocking._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalatest.prop.Checkers

class BlockingSpec extends CommonSpec with Checkers {

  implicit val genExecutorService: Gen[ExecutorService] =
    for {
//      nThreads <- Gen.choose(1, 10) // for a small number of threads the fork implementation will sometimes deadlock
      nThreads <- Gen.const(32)
    } yield Executors.newFixedThreadPool(nThreads)

  implicit val arbExecutorService: Arbitrary[ExecutorService] =
    Arbitrary(genExecutorService)

  // The toString method of ThreadPoolExecutor reflects the internal state of the ExecutorService
  // so we try to use it for the Cogen
  implicit val cogenExecutorService: Cogen[ExecutorService] =
    Cogen[String].contramap(_.toString)

  implicit def genFuture[A: Arbitrary]: Gen[Future[A]] =
    for {
      x <- implicitly[Arbitrary[A]].arbitrary
    } yield CompletableFuture.completedFuture(x)

  implicit def arbFuture[A: Arbitrary]: Arbitrary[Future[A]] =
    Arbitrary(genFuture[A])

  implicit def arbPar[A: Arbitrary]: Arbitrary[Par[A]] =
    Arbitrary.arbFunction1[ExecutorService, Future[A]]

  "map2" should "be" in {
    forAll { (a: Par[Int], b: Par[Int], f: (Int, Int) => Int, es: ExecutorService) =>
      map2(a, b)(f)(es).get() mustBe f(a(es).get(), b(es).get())
    }
  }

  "map" should "be" in {
    forAll { (p: Par[Int], es: ExecutorService) =>
      map(p)(identity)(es).get() mustBe p(es).get()
    }
  }

  "map" should "have the fusion property" in {
    forAll { (pa: Par[Int], f: Int => Int, g: Int => Int, a: Int, es: ExecutorService) =>
      map(map(pa)(g))(f)(es).get() mustBe map(pa)(f compose g)(es).get()
    }
  }

  "fork x" should "be x" in {
    forAll { (x: Int, es: ExecutorService) =>
      fork(unit(x))(es).get() mustBe unit(x)(es).get()
    }
  }

  // ex 7.4
  "asyncF" should "be" in {
    forAll { (a: Int, f: Int => Int, es: ExecutorService) =>
      asyncF(f)(a)(es).get() mustBe f(a)
    }
  }

  // ex 7.5
  "sequence" should "be" in {
    forAll(Gen.listOfN(20, arbPar[Int].arbitrary),
           Gen.listOfN(20, arbPar[Int].arbitrary),
           genExecutorService) {
      case (xs, ys, es: ExecutorService) =>
        sequence(xs ++ ys)(es)
          .get() mustBe map2(sequence(xs), sequence(ys))(_ ++ _)(es).get()
    }
  }

  "parMap" should "be" in {
    forAll(Gen.listOf(Arbitrary.arbInt.arbitrary),
           Arbitrary.arbFunction1[Int, Int].arbitrary,
           genExecutorService) {
      case (xs, f, es) =>
        parMap(xs)(f)(es).get() mustBe xs.map(f)
    }
  }

  // ex 7.6
  "parFilter" should "be" in {
    forAll(Gen.listOf(Arbitrary.arbInt.arbitrary),
           Arbitrary.arbFunction1[Int, Boolean].arbitrary,
           genExecutorService) {
      case (xs, f, es) =>
        parFilter(xs)(f)(es).get() mustBe xs.filter(f)
    }
  }

  "sum" should "be" in {
    forAll(Gen.listOfN(10, Arbitrary.arbInt.arbitrary), genExecutorService) {
      case (xs, es) =>
        sum(xs.toIndexedSeq)(es).get() mustBe xs.sum
    }
  }

  // p. 110
  "reduce" should "be" in {
    forAll(Gen.listOfN(10, Arbitrary.arbInt.arbitrary),
           Arbitrary.arbInt.arbitrary,
           genExecutorService) {
      case (xs, z, es) =>
        reduce(xs.toIndexedSeq, z)(_ + _)(es)
          .get() mustBe xs.toIndexedSeq.foldRight(z)(_ + _)
    }
  }

  "maximum" should "be like max" in {
    forAll(Gen.listOfN(10, Arbitrary.arbInt.arbitrary), genExecutorService) {
      case (xs, es) =>
        maximum(xs.toIndexedSeq).fold(())(x => x(es).get() mustBe xs.max)
    }
  }

  "countWords" should "be" in {
    forAll(Gen.listOfN(10, Arbitrary.arbString.arbitrary), genExecutorService) {
      case (paragraphs, es) =>
        countWords(paragraphs)(es).get() mustBe
          paragraphs.foldRight(0)((paragraph, count) =>
            count + paragraph.split("\\W+").length)
    }
  }

  "map3" should "be" in {
    forAll { (a: Par[Int], b: Par[Int], c: Par[Int], f: (Int, Int, Int) => Int, es: ExecutorService) =>
      map3(a, b, c)(f)(es).get() must
        (equal(f(a(es).get(), b(es).get(), c(es).get())) and
          equal(map3_v2(a, b, c)(f)(es).get()))
    }
  }

  "map4" should "be" in {
    (a: Par[Int],
     b: Par[Int],
     c: Par[Int],
     d: Par[Int],
     f: (Int, Int, Int, Int) => Int,
     es: ExecutorService) =>
      map4(a, b, c, d)(f)(es).get() mustBe f(a(es).get(),
                                             b(es).get(),
                                             c(es).get(),
                                             d(es).get())

  }

  "map5" should "be" in {
    val genArgs: Gen[(Par[Int],
                      Par[Int],
                      Par[Int],
                      Par[Int],
                      Par[Int],
                      (Int, Int, Int, Int, Int) => Int,
                      ExecutorService)] =
      for {
        a <- arbPar[Int].arbitrary
        b <- arbPar[Int].arbitrary
        c <- arbPar[Int].arbitrary
        d <- arbPar[Int].arbitrary
        e <- arbPar[Int].arbitrary
        f <- Arbitrary.arbFunction5[Int, Int, Int, Int, Int, Int].arbitrary
        es <- arbExecutorService.arbitrary
      } yield (a, b, c, d, e, f, es)
    forAll(genArgs) {
      case (a, b, c, d, e, f, es) =>
        map5(a, b, c, d, e)(f)(es).get() mustBe f(a(es).get(),
                                                  b(es).get(),
                                                  c(es).get(),
                                                  d(es).get(),
                                                  e(es).get())

    }
  }

}
