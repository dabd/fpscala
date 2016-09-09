package fpinscala

import scala.annotation.tailrec

object DataStructures {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    // ex 3.2
    def tail[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("cannot take tail of empty list")
      case Cons(_, t) => t
    }

    // ex 3.3
    def setHead[A](l: List[A], h: A): List[A] = l match {
      case Nil => sys.error("cannot set head of empty list")
      case Cons(_, t) => Cons(h, t)
    }

    // ex 3.4
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
      case (_, 0) => l
      case (Nil, _) => Nil
      case (Cons(_, t), i) => drop(t, i - 1)
    }

    def length2[A](l: List[A]): Int = {
      @tailrec
      def rec[A](l: List[A], acc: Int): Int = l match {
        case Nil => acc
        case Cons(_, t) => rec(t, acc + 1)
      }
      rec(l, 0)
    }

    // ex: 3.5
    @tailrec
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => l
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    }

    def append[A](xs: List[A], ys: List[A]): List[A] = xs match {
      case Nil => ys
      case Cons(h, t) => Cons(h, append(t, ys))
    }

    def reverse[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) => append(reverse(t), Cons(h, Nil))
    }

    // ex: 3.6
    def init[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("cannot take init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    // ex: 3.9
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(h, t) => f(h, foldRight(t, z)(f))
      }

    def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  }
}
