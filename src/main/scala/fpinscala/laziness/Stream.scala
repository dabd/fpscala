package fpinscala.laziness

sealed trait Stream[+A] {

  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // ex 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // ex 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def length: Int = this match {
    case Empty => 0
    case Cons(_, t) => 1 + t().length
  }

  def append[AA >: A](that: Stream[AA]): Stream[AA] = this match {
    case Empty => that
    case Cons(h, t) => cons(h(), t().append(that))
  }

  // ex 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  // ex 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // ex 5.5
  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  // ex 5.6
  def headOptionWithFoldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // ex 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def appendWithFoldRight[AA >: A](that: => Stream[AA]): Stream[AA] =
    foldRight(that)(cons(_, _))

  def flatMap[AA >: A](f: AA => Stream[AA]): Stream[AA] =
    foldRight(empty[AA])((a, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  // ex 5.13
  def unfoldMap[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }

  def unfoldTake(n: Int): Stream[A] =
    unfold((n, this)) {
      case (m, Cons(h, t)) if m > 0 => Some(h(), (m - 1, t()))
      case _ => None
    }

  def unfoldTakeWhile(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def unfoldZipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAll[B, C](that: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
      case (Empty, Empty) => None
    }

  // ex 5.14
  def startsWith[AA >: A](that: Stream[AA]): Boolean =
    unfoldZipWith(that)((_, _)).forAll(x => x._1 == x._2)

  // ex 5.15
  def tails: Stream[Stream[A]] =
    cons(this, unfold(this) {
      case Cons(_, t) => Some(t(), t())
      case Empty => None
    })

  // ex 5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    tails.map(_.foldRight(z)(f))

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def fromList[A](l: List[A]): Stream[A] = l match {
    case Nil => empty[A]
    case h :: t => cons(h, fromList(t))
  }

  val ones: Stream[Int] = Stream.cons(1, ones)

  // ex 5.8
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  // ex 5.9
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  // ex 5.10
  import fpinscala.gettingstarted.GettingStarted.fib

  def fibs(n: BigInt): Stream[BigInt] = {
    cons(fib(n), fibs(n + 1))
  }

  // ex 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty[A]
  }

  // ex 5.12
  def unfoldFibs(n: BigInt) = unfold((fib(n), fib(n + 1))) {
    case (a, s) => Some(a, (s, a + s))
  }

  def unfoldFrom(n: Int) =
    unfold(n)(x => Some(x, x + 1))

  def unfoldConstant[A](a: A): Stream[A] =
    unfold(a)(Some(a, _))

  val unfoldOnes: Stream[Int] = unfold(1)(Some(1, _))

}
