package fpinscala

import org.scalacheck._
import GettingStarted._


object GettingStartedSpec extends Properties("GettingStartedSpec") {

  import org.scalacheck.Prop.forAll

  property("fibonacci") = forAll(Arbitrary.arbBigInt.arbitrary.suchThat(n => n >= 0 && n <= Int.MaxValue)) { n =>
    val golden = (1 + math.sqrt(5)) / 2
    val f = Math.round(math.pow(golden, n.toDouble) / math.sqrt(5))
    fib(n) == f
  }

  property("is sorted") = forAll { a: Array[Int] =>
    isSorted(a, (x: Int, y: Int) => x <= y) == isSorted(a.reverse, (x: Int, y: Int) => x >= y)
  }

}
