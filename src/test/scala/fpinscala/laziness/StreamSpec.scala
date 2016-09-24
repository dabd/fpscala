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

  "foldRight" should "be" in forAll {
    (xs: Stream[Int], z: Int, f: (Int, Int) => Int) =>
      def g(a: Int, b: => Int): Int = f(a, b)

      xs.foldRight(z)(g) mustBe xs.toList.foldRight(z)(f)
  }

  "forAll" should "be" in forAll { (xs: Stream[Int], p: Int => Boolean) =>
    xs.forAll(p) mustBe xs.toList.forall(p)
  }

  "takeWhileWithFoldRight" should "be equal to foldRight" in forAll {
    (xs: Stream[Int], p: Int => Boolean) =>
      xs.takeWhileWithFoldRight(p).toList mustBe xs.takeWhile(p).toList
  }

  "headOptionWithFoldRight" should "be equal to headOption" in forAll {
    xs: Stream[Int] =>
      xs.headOptionWithFoldRight mustBe xs.headOption
  }

  "map" should "be" in forAll { (xs: Stream[Int], f: Int => Int) =>
    xs.map(f).toList mustBe xs.toList.map(f)
  }

  "filter" should "be" in forAll { (xs: Stream[Int], p: Int => Boolean) =>
    xs.filter(p).toList mustBe xs.toList.filter(p)
  }

  "appendWithFoldRight" should "be" in forAll {
    (xs: Stream[Int], ys: Stream[Int]) =>
      xs.appendWithFoldRight(ys).toList mustBe xs.append(ys).toList
  }

  "flatMap of Stream" should "be equal to flatMap of List" in forAll {
    (xs: Stream[Int], f: Int => Stream[Int]) =>
      def g(x: Int): List[Int] = f(x).toList

      xs.flatMap(f).toList mustBe xs.toList.flatMap(g)
  }
}
