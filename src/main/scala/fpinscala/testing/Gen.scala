package fpinscala.testing

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.laziness.Stream
import fpinscala.state.{RNG, State}
import fpinscala.testing.Gen._
import fpinscala.testing.Prop.{apply => _, _}
import fpinscala.parallelism.Blocking.Par

// ex 8.3
//trait Prop {
//  def check: Boolean
//
//  def &&(p: Prop): Prop = new Prop {
//    def check: Boolean = Prop.this.check && p.check
//  }
//}

case class Gen[+A](sample: State[RNG, A]) {

  // ex 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g)((_, _))

  // ex 8.10
  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {

  // ex 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(rng =>
      RNG.nonNegativeInt(rng) match {
        case (n, rng) => (start + n % (stopExclusive - start), rng)
    }))

  // ex 8.5
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(rng =>
      RNG.nonNegativeLessThan(2)(rng) match {
        case (0, rng) => (false, rng)
        case (1, rng) => (true, rng)
    }))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def listOfNv2[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State(rng => ((1 to n).map(_ => g.sample.run(rng)._1).toList, rng)))

  // ex 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  // ex 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val w1 = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(w =>
      if (w <= w1) g1._1.sample else g2._1.sample))
  }

  def weightedv2[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (_g1, w1) = g1
    val (_g2, w2) = g2
    // normalise and scale weights to ints in [0, 100]
    val normW1 = w1.abs / (w1.abs + w2.abs) * 100

    choose(0, 100).flatMap(w => if (w <= normW1) _g1 else _g2)
  }

}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop) = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case x => x
    }
  }

  def ||(p: Prop) = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      // In case of failure, run the other prop.
      case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
      case x => x
    }
  }

  /* This is rather simplistic - in the event of failure, we simply prepend
   * the given message on a newline in front of the existing message.
   */
  def tag(msg: String) = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount)
      extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    Prop { (n, rng) =>
      randomStream(as)(rng)
        .zip(Stream.from(0))
        .take(n)
        .map {
          case (a, i) =>
            try {
              if (f(a)) Passed else Falsified(a.toString, i)
            } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
        }
        .find(_.isFalsified)
        .getOrElse(Passed)
    }

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) =>
      f(n, rng)
    }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props
          .map(p =>
            Prop { (max, n, rng) =>
              p.run(max, casesPerSize, rng)
          })
          .toList
          .reduce(_ && _)
      prop.run(max, n, rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen { g(_) map f }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      g(n) flatMap { f(_).g(n) }
    }
    SGen(g2)
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] =
    SGen(n => apply(n) ** s2(n))
}
