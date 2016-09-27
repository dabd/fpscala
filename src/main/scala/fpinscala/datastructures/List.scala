package fpinscala.datastructures

import scala.annotation.tailrec
import scala.collection.immutable

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def toScalaList[A](l: List[A]): immutable.List[A] =
    foldRight(l, immutable.Nil: immutable.List[A])((a, acc) => a :: acc)

  def fromScalaList[A](l: immutable.List[A]): List[A] =
    l.foldRight(Nil: List[A])((a, acc) => Cons(a, acc))

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

  // ex 3.5
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def takeWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, takeWhile(t, f)) else Nil
  }

  def append[A](xs: List[A], ys: List[A]): List[A] = xs match {
    case Nil => ys
    case Cons(h, t) => Cons(h, append(t, ys))
  }

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => append(reverse(t), Cons(h, Nil))
  }

  // ex 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("cannot take init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // ex 3.9
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  def length2[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  // ex 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  // ex 3.11
  def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def length3(l: List[Int]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  // ex 3.12
  def reverse2[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))

  // ex 3.13
  def foldLeftInTermsOfFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRightInTermsOfFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  // ex 3.14
  def append2[A](xs: List[A], ys: List[A]): List[A] =
    foldRight(xs, ys)(Cons(_, _))

  /*
    ex 3.15

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
  def flatten[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(append)

  // ex 3.16
  def add1Recursive(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, add1Recursive(t))
  }

  /*
    Using tranformation steps from section 3.3 of http://www.cs.nott.ac.uk/~pszgmh/fold.pdf

    add1Recursive = fold f v

    add1Recursive [] = v
    add1Recursive (x : xs) = f x (add1Recursive xs)

    v = []

    add1Recursive (x : xs) = f x (add1Recursive xs)
    (x + 1) : add1Recursive xs = f x (add1Recursive xs)
    (x + 1) : y = f x y
    f x y = (x + 1) : y

    f = (:) . (1 +)
   */

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((a, acc) => Cons(a + 1, acc))

  // ex 3.17
  def listDoubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((d, acc) => Cons(d.toString, acc))

  // ex 3.18
  def mapRecursive[A, B](as: List[A], f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), mapRecursive(t, f))
  }

  /*
    mapRecursive f = fold g v

    mapRecursive f [] = v
    mapRecursive f (x : xs) = g x (mapRecursive f xs)

    v = []

    mapRecursive (x : xs) = g x (mapRecursive f xs)
    (f x) : (mapRecursive f xs) = g x (mapRecursive f xs)
    (f x) : y = g x y

    g = (:) . f
   */

  def map[A, B](as: List[A], f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, acc) => Cons(f(a), acc))

  // ex 3.19
  def filterRecursive[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) Cons(h, filterRecursive(t)(f)) else filterRecursive(t)(f)
  }

  /*
    filterRecursive f = fold g v
    filterRecursive f [] = v
    filterRecursive f (x : xs) = g x (filterRecursive f xs)

    v = []

    filterRecursive f (x : xs) = g x (filterRecursive f xs)
    if (f x) x : filterRecursive f xs else filterRecursive f xs = g x filterRecursive f xs
    if (f x) x : y else y = g x y

    g = \x y -> if (f x) x : y else y
   */

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

  // ex 3.20
  def flatMapRecursive[A, B](as: List[A])(f: A => List[B]): List[B] =
    as match {
      case Nil => Nil
      case Cons(h, t) => append(f(h), flatMapRecursive(t)(f))
    }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((a, acc) => append(f(a), acc))

  def flatMap2[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])(
      Function.uncurried((append[B] _).curried.compose(f)))

  // ex 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  // ex 3.22
  def addListsRecursive(as: List[Int], bs: List[Int]): List[Int] =
    (as, bs) match {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Cons(h1 + h2, addListsRecursive(t1, t2))
      case _ => Nil
    }

  def addLists(as: List[Int], bs: List[Int]): List[Int] =
    zipWith(as, bs, (a: Int, b: Int) => a + b)

  def nth[A](n: Int, as: List[A]): Option[A] = (as, n) match {
    case (Nil, _) => None
    case (_, n) if n < 0 => None
    case (Cons(h, _), 0) => Some(h)
    case (Cons(h, t), n) => nth(n - 1, t)
  }

  // ex 3.23
  def zipWithRecursive[A, B, C](as: List[A],
                                bs: List[B],
                                f: (A, B) => C): List[C] = (as, bs) match {
    case (Cons(h1, t1), Cons(h2, t2)) =>
      Cons(f(h1, h2), zipWithRecursive(t1, t2, f))
    case _ => Nil
  }

  // From zipWith2 http://lpaste.net/revision/47814
  // Also foldRight can be curried for a nicer syntax when calling foldRight http://scastie.org/22284
  def zipWith[A, B, C](as: List[A], bs: List[B], f: (A, B) => C): List[C] = {
    def nil(bs: List[B]): List[C] = Nil
    def cons(a: A)(zipsFn: List[B] => List[C])(bs: List[B]): List[C] =
      bs match {
        case Nil => Nil
        case Cons(h, t) => Cons(f(a, h), zipsFn(t))
      }
    foldRight(as, nil _)(cons(_)(_))(bs)
  }

  // ex 3.24
  def startsWith[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
      case _ => false
    }

  def existsTail[A](as: List[A], p: List[A] => Boolean): Boolean = as match {
    case Nil => false
    case Cons(_, t) => if (p(as)) true else existsTail(t, p)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ => existsTail(sup, startsWith(_: List[A], sub))
  }

}
