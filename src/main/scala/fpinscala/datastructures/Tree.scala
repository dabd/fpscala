package fpinscala.datastructures

sealed trait Tree[+A] {
  def +[B >: A](that: Tree[B]): Tree[B] = this match {
    case Leaf(v) => Branch(this, that)
    case Branch(l, r) => Branch(l, that + r)
  }
}

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def left[A](t: Tree[A]): Tree[A] = t match {
    case Leaf(_) => sys.error("can't take left branch of leaf")
    case Branch(l, _) => l
  }

  def right[A](t: Tree[A]): Tree[A] = t match {
    case Leaf(_) => sys.error("can't take right branch of leaf")
    case Branch(_, r) => r
  }

  // ex 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // ex 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // ex 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // ex 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // ex 3.29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeUsingFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximumUsingFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthUsingFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((d1, d2) => 1 + (d1 max d2))

  def mapUsingFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}
