package fpinscala

object DataStructures {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    // Ex 3.2
    def tail[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("cannot take tail of empty list")
      case Cons(_, t) => t
    }

    // Ex 3.3
    def setHead[A](l: List[A], h: A): List[A] = l match {
      case Nil => sys.error("cannot set head of empty list")
      case Cons(_, t) => Cons(h, t)
    }

    // Ex 3.4
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
      case (_, 0) => l
      case (Nil, _) => Nil
      case (Cons(_, t), i) => drop(t, i - 1)
    }

    def drop2[A](l: List[A], n: Int): List[A] = n match {
      case 0 => l
      case i if l == Nil => Nil
      case i if l != Nil => drop2(tail(l), i - 1)
    }

    def length[A](l: List[A]): Int = {
      def rec[A](l: List[A], acc: Int): Int = l match {
        case Nil => acc
        case Cons(_, t) => rec(t, acc + 1)
      }
      rec(l, 0)
    }
  }
}
