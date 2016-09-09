package fpinscala

import fpinscala.GettingStarted._
import org.scalacheck._

object GettingStartedSpec extends Properties("GettingStartedSpec") {

  import org.scalacheck.Prop.forAll

  property("fibonacci") = forAll(
    Arbitrary.arbBigInt.arbitrary.suchThat(n => n >= 0 && n <= Int.MaxValue)) {
    n =>
      val golden = (1 + math.sqrt(5)) / 2
      val f = Math.round(math.pow(golden, n.toDouble) / math.sqrt(5))
      fib(n) == f
  }

  property("isSorted") = forAll { a: Array[Int] =>
    isSorted(a, (x: Int, y: Int) => x <= y) == isSorted(a.reverse,
                                                        (x: Int,
                                                         y: Int) => x >= y)
  }

  property("isSorted2") = forAll { a: Array[Int] =>
    isSorted2(a, (x: Int, y: Int) => x <= y) == isSorted2(a.reverse,
                                                          (x: Int,
                                                           y: Int) => x >= y)
  }

  // TODO: add tests for curry, uncurry and compose
  // https://gitter.im/typelevel/cats?at=57c5b7d065053b006ce0f3d3
  // https://gitter.im/typelevel/cats?at=57caebce29ee4a67058b5ec6

  import Arbitrary.{arbFunction1, arbFunction2}

  property("curry") = forAll { (f: (Int, Int) => Int, x: Int, y: Int) =>
    curry(f)(x)(y) == f(x, y)
  }

  property("uncurry") = forAll { (f: Int => Int => Int, x: Int, y: Int) =>
    uncurry(f)(x, y) == f(x)(y)
  }

  property("compose") = forAll { (f: Int => Int, g: Int => Int, x: Int) =>
    compose(f, g)(x) == f(g(x))
  }

}
