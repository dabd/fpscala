package fpinscala.parallelism

import java.util.concurrent._

import fpinscala.CommonSpec
import fpinscala.parallelism.Par.Par
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalatest.prop.Checkers

class ParSpec extends CommonSpec with Checkers {

  implicit val genExecutorService: Gen[ExecutorService] =
    Gen.const(Executors.newFixedThreadPool(4))

  implicit val arbExecutorService: Arbitrary[ExecutorService] =
    Arbitrary(genExecutorService)

  // The toString method of ThreadPoolExecutor reflects the internal state of the ExecutorService
  // so we try to use it for the Cogen
  implicit val cogenExecutorService: Cogen[ExecutorService] =
    Cogen[String].contramap(_.toString)

  implicit def genFuture[A](implicit a: Arbitrary[A]): Gen[Future[A]] =
    for {
      x <- a.arbitrary
    } yield CompletableFuture.completedFuture(x)

  implicit def arbFuture[A](implicit a: Arbitrary[A]): Arbitrary[Future[A]] =
    Arbitrary(genFuture)

  implicit def cogenFuture[A](implicit a: Cogen[A]): Cogen[Future[A]] =
    Cogen[String].contramap(_.toString)

  implicit def arbitraryPar[A: Arbitrary]: Arbitrary[Par[A]] =
    Arbitrary.arbFunction1[ExecutorService, Future[A]]

  // ex 7.4
  "asyncF" should "be" in {
    forAll { (a: Int, f: Int => Int, es: ExecutorService) =>
      Par.asyncF(f)(a)(es).get() mustBe f(a)
    }
  }

  // ex 7.5
  "sequence" should "be" in {
    forAll(Gen.listOfN(20, arbitraryPar[Int].arbitrary),
           Gen.listOfN(20, arbitraryPar[Int].arbitrary),
           genExecutorService) {
      case (xs, ys, es: ExecutorService) =>
        Par.sequence(xs ++ ys)(es).get() mustBe Par
          .map2(Par.sequence(xs), Par.sequence(ys))(_ ++ _)(es)
          .get()

    }
  }

  "parMap" should "be" in {
    forAll(Gen.listOf(Arbitrary.arbInt.arbitrary),
      Arbitrary.arbFunction1[Int, Int].arbitrary,
      genExecutorService) {
      case (xs, f, es) =>
        Par.parMap(xs)(f)(es).get() mustBe xs.map(f)
    }
  }

  // ex 7.6
  "parFilter" should "be" in {
    forAll(Gen.listOf(Arbitrary.arbInt.arbitrary),
      Arbitrary.arbFunction1[Int, Boolean].arbitrary,
      genExecutorService) {
      case (xs, f, es) =>
        Par.parFilter(xs)(f)(es).get() mustBe xs.filter(f)
    }
  }
}
