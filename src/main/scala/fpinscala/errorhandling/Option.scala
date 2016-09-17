package fpinscala.errorhandling

import scala.{Either => _, Option => _}

sealed trait Option[+A] {
  // ex 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(v) => Some(v)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(v) => if (f(v)) Some(v) else None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // ex 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def variance2(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m =>
      mean(xs.map(x => math.pow(x, 2))).map(m2 => m2 - math.pow(m, 2)))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  // ex 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }

  def map2_2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  // ex 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil): Option[List[A]])(map2(_, _)(_ :: _))

  def sequenceUsingTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  // ex 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((a, acc) =>
      map2(f(a), acc)(_ :: _))

}
