package fpinscala.laziness

import fpinscala.CommonSpec
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._

class StreamSpec extends CommonSpec {

  implicit def arbStream[T: Arbitrary]: Arbitrary[Stream[T]] = Arbitrary {
    val genEmpty = Gen.const(Stream.empty[T])

    def genCons(sz: Int): Gen[Stream[T]] =
      for {
        x <- arbitrary[T]
        xs <- sizedStream(sz / 2)
      } yield Cons(() => x, () => xs)

    def sizedStream(sz: Int) =
      if (sz <= 0) genEmpty
      else Gen.frequency((1, genEmpty), (3, genCons(sz)))

    Gen.sized(sz => sizedStream(sz))
  }

  "toList" should "be" in forAll { l: List[Int] =>
    Stream.fromList(l).toList mustBe l
  }

  "append" should "be associative" in forAll {
    (xs: Stream[Int], ys: Stream[Int]) =>
      xs.append(ys).toList mustBe xs.toList ::: ys.toList
  }

  "length" should "be" in forAll { xs: Stream[Int] =>
    xs.length mustBe xs.toList.length
  }

  "take" should "be" in forAll { (xs: Stream[Int], n: Int) =>
    xs.take(n).toList mustBe xs.toList.take(n)
  }

  "drop" should "be" in forAll { (xs: Stream[Int], n: Int) =>
    xs.drop(n).toList mustBe xs.toList.drop(n)
  }

  "takeWhile" should "be" in forAll { (xs: Stream[Int], p: Int => Boolean) =>
    xs.takeWhile(p).toList mustBe xs.toList.takeWhile(p)
  }
}
