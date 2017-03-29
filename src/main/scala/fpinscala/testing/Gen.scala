package fpinscala.testing

import fpinscala.state.{RNG, State}

// ex 8.3
trait Prop {
  def check: Boolean

  def &&(p: Prop): Prop = new Prop {
    def check: Boolean = Prop.this.check && p.check
  }
}

case class Gen[A](sample: State[RNG, A]) {

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

  def listOfN2[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State(rng => ((1 to n).map(_ => g.sample.run(rng)._1).toList, rng)))

}
