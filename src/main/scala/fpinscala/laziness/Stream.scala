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
}
