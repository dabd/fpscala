package fpinscala.testing

import fpinscala.state.{RNG, State}

// ex 8.3
trait Prop {
  def check: Boolean

  def &&(p: Prop): Prop = new Prop {
    def check: Boolean = Prop.this.check && p.check
  }
}

case class
Gen[+A](sample: State[RNG, A]) {

  // ex 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))
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
