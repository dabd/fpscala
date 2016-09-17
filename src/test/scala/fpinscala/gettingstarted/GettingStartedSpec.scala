package fpinscala.gettingstarted

import fpinscala.CommonSpec
import fpinscala.gettingstarted.GettingStarted._
import org.scalacheck._

class GettingStartedSpec extends CommonSpec {

  "fibonacci" should "be compute the same values as the closed formula" in forAll(
    Gen.choose(0, 30)) { n =>
    fib(n) mustBe fibRecursive(n)
  }

  "isSorted" should "be return the same value on a reversed array" in forAll {
    a: Array[Int] =>
      isSorted(a, (x: Int, y: Int) => x <= y) mustBe isSorted(
        a.reverse,
        (x: Int, y: Int) => x >= y)
  }

  "isSorted2" should "be equal to isSorted" in forAll { a: Array[Int] =>
    isSorted2(a, (x: Int, y: Int) => x <= y) mustBe isSorted(
      a,
      (x: Int, y: Int) => x >= y)
  }

  // https://gitter.im/typelevel/cats?at=57c5b7d065053b006ce0f3d3
  // https://gitter.im/typelevel/cats?at=57caebce29ee4a67058b5ec6

  import Arbitrary.{arbFunction1, arbFunction2}

  "curry" should "be" in forAll { (f: (Int, Int) => Int, x: Int, y: Int) =>
    curry(f)(x)(y) mustBe f(x, y)
  }

  "uncurry" should "be" in forAll { (f: Int => Int => Int, x: Int, y: Int) =>
    uncurry(f)(x, y) mustBe f(x)(y)
  }

  "compose" should "be" in forAll { (f: Int => Int, g: Int => Int, x: Int) =>
    compose(f, g)(x) mustBe f(g(x))
  }

}
