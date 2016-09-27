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

  "append for Stream" should "be equal to append for List" in forAll {
    (xs: Stream[Int], ys: Stream[Int]) =>
      xs.append(ys).toList mustBe xs.toList ::: ys.toList
  }

  "length for Stream" should "be equal to length for List" in forAll {
    xs: Stream[Int] =>
      xs.length mustBe xs.toList.length
  }

  "take for Stream" should "be equal to take for List" in forAll {
    (xs: Stream[Int], n: Int) =>
      xs.take(n).toList mustBe xs.toList.take(n)
  }

  "drop for Stream" should "be equal to drop for List" in forAll {
    (xs: Stream[Int], n: Int) =>
      xs.drop(n).toList mustBe xs.toList.drop(n)
  }

  "takeWhile from Stream" should "be equal to takeWhile from List" in forAll {
    (xs: Stream[Int], p: Int => Boolean) =>
      xs.takeWhile(p).toList mustBe xs.toList.takeWhile(p)
  }

  "foldRight a Stream" should "be equal to foldRight a List" in forAll {
    (xs: Stream[Int], z: Int, f: (Int, Int) => Int) =>
      def g(a: Int, b: => Int): Int = f(a, b)

      xs.foldRight(z)(g) mustBe xs.toList.foldRight(z)(f)
  }

  "forAll for Stream" should "be equal to forall for List" in forAll {
    (xs: Stream[Int], p: Int => Boolean) =>
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

  "map a Stream" should "be equal to map a List" in forAll {
    (xs: Stream[Int], f: Int => Int) =>
      xs.map(f).toList mustBe xs.toList.map(f)
  }

  "filter a Stream" should "be equal to filter a List" in forAll {
    (xs: Stream[Int], p: Int => Boolean) =>
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

  "find for Stream" should "be equal to find for List" in forAll {
    (xs: Stream[Int], p: Int => Boolean) =>
      xs.find(p) mustBe xs.toList.find(p)
  }

  val genConstAndSize = for {
    a <- Arbitrary.arbInt.arbitrary
    n <- Gen.choose(0, 50)
  } yield (a, n)

  "constant" should "be" in forAll(genConstAndSize) {
    case (a, n) =>
      Stream.constant(a).take(n).forAll(_ == a) mustBe true
  }

  "from" should "be" in forAll {
    for {
      n <- Gen.choose(-500, 500)
      sz <- Gen.choose(1, 50)
    } yield (n, sz)
  } {
    case (n, sz) =>
      whenever(sz > 0) {
        Stream.from(n).take(sz).toList mustBe n.until(n + sz).toList
      }
  }

  val genNandSize = for {
    n <- Gen.choose(1, 30)
    m <- Gen.const(30 - n)
  } yield (n, m)

  "fibs" should "be" in forAll(genNandSize) {
    case (n, m) =>
      whenever(m > 0) {
        import fpinscala.gettingstarted.GettingStarted.fib
        Stream.fibs(n).take(m).toList mustBe n.until(n + m).map(fib(_)).toList
      }
  }

  // idea to PBT unfold https://gist.github.com/raichoo/53ed5619988c0b4dd590/

  "unfoldFibs" should "be" in forAll(genNandSize) {
    case (n, m) =>
      Stream.unfoldFibs(n).take(m).toList mustBe Stream.fibs(n).take(m).toList
  }

  "unfoldFrom" should "be" in forAll(genNandSize) {
    case (n, m) =>
      Stream.unfoldFrom(n).take(m).toList mustBe Stream.from(n).take(m).toList
  }

  "unfoldConstant" should "be" in forAll(genConstAndSize) {
    case (a, n) =>
      Stream.unfoldConstant(a).take(n).toList mustBe Stream
        .constant(a)
        .take(n)
        .toList
  }

  "unfoldOnes" should "be" in forAll(Gen.choose(0, 100)) { n =>
    Stream.unfoldOnes.take(n).toList mustBe Stream.unfoldOnes.take(n).toList
  }

  "unfoldMap" should "be equal to map" in forAll { (xs: Stream[Int], f: Int => Int) =>
    xs.unfoldMap(f).toList mustBe xs.map(f).toList
  }

  "unfoldTake" should "be equal to take" in forAll {
    (xs: Stream[Int], n: Int) =>
      xs.unfoldTake(n).toList mustBe xs.take(n).toList
  }

  "unfoldTakeWhile" should "be equal to takeWhile" in forAll {
    (xs: Stream[Int], p: Int => Boolean) =>
      xs.unfoldTakeWhile(p).toList mustBe xs.takeWhile(p).toList
  }

  "unfoldZipWith" should "be equal to zipWith" in forAll {
    (xs: Stream[Int], ys: Stream[Int], f: (Int, Int) => Int) =>
      import fpinscala.datastructures.List.zipWith
      import fpinscala.datastructures.List.fromScalaList
      import fpinscala.datastructures.List.toScalaList
      xs.unfoldZipWith(ys)(f).toList mustBe toScalaList(
        zipWith(fromScalaList(xs.toList), fromScalaList(ys.toList), f))
  }

  "zipAll" should "be" in forAll { (xs: Stream[Int], ys: Stream[Int]) =>
    val (as, bs) = xs.zipAll(ys).toList.unzip
    as.takeWhile(_.isDefined).map(_.get) mustBe xs.toList
    bs.takeWhile(_.isDefined).map(_.get) mustBe ys.toList
    math.abs(xs.length - ys.length) mustBe (as
      .dropWhile(_.isDefined)
      .length max bs.dropWhile(_.isDefined).length)
  }

  val genMaybeSupSub = for {
    xs <- arbStream[Int].arbitrary
    ys <- arbStream[Int].arbitrary
    zs <- Gen
      .frequency((9, Gen.const(xs.append(ys), xs)), (1, Gen.const((xs, ys))))
  } yield zs

  "startsWith for Stream" should "be equal to startsWith for List" in forAll(
    genMaybeSupSub) {
    case (sup, sub) =>
      sup.startsWith(sub) mustBe sup.toList.startsWith(sub.toList)
  }

  "tails" should "be" in forAll { xs: Stream[Int] =>
    xs.tails.map(_.toList).toList mustBe xs.toList.tails.toList
  }

  "scanRight" should "be" in forAll {
    (xs: Stream[Int], z: Int, f: (Int, Int) => Int) =>
      def g(a: Int, b: => Int): Int = f(a, b)

      xs.scanRight(z)(g).toList mustBe xs.toList.scanRight(z)(f)
  }

}
