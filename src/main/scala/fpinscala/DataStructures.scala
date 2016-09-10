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

    def length[A](l: List[A]): Int = {
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

    def length2[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

    // ex: 3.10
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
      as match {
        case Nil => z
        case Cons(h, t) => foldLeft(t, f(z, h))(f)
      }

    // ex: 3.11
    def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

    def product(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

    def length3(l: List[Int]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

    // ex: 3.12
    def reverse2[A](l: List[A]): List[A] =
      foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))

    // ex: 3.13
    def foldLeftInTermsOfFoldRight[A, B](as: List[A], z: B)(f: (B,
                                                                A) => B): B =
      foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

    def foldRightInTermsOfFoldLeft[A, B](as: List[A], z: B)(f: (A,
                                                                B) => B): B =
      foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

    // ex: 3.14
    def append2[A](xs: List[A], ys: List[A]): List[A] =
      foldRight(xs, ys)(Cons(_, _))

    /*
    ex: 3.15

    Transforming the recursive definition of flatten into a fold using section 3.3 from http://www.cs.nott.ac.uk/~pszgmh/fold.pdf

    flatten [] = []
    flatten (x : xs) = x ++ flatten xs

    flatten = fold f v

    flatten [] = v
    flatten (x : xs) = f x (flatten xs)

    v = []

    flatten (x : xs) = f x (flatten xs)
    x ++ flatten xs = f x (flatten xs)
    x ++ y = f x y

    f = (++)

    flatten = fold (++) []
    */
    def flatten[A](as: List[List[A]]): List[A] = foldRight(as, Nil: List[A])(append)

    // ex: 3.16
    def add1Recursive(l: List[Int]): List[Int] = l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h + 1, add1Recursive(t))
    }

    /*
    Using tranformation steps from section 3.3 of http://www.cs.nott.ac.uk/~pszgmh/fold.pdf

    add1Recursive = fold f v

    add1Recursive [] = v
    add1Recursive (x: xs) = f x (add1Recursive xs)

    v = []

    add1Recursive (x : xs) = f x (add1Recursive xs)
    (x + 1) : add1Recursive xs = f x (add1Recursive xs)
    (x + 1) : y = f x y
    f x y = (x + 1) : y
    f = (:) . (1 +)
     */

    def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((a, acc) => Cons(a + 1, acc))

    //ex: 3.17
    def listDoubleToListString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((d, acc) => Cons(d.toString, acc))
  }
}
