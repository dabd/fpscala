package fpinscala.errorhandling

import scala.{Either => _}

sealed trait Either[+E, +A] {
  // ex 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(aa => b map (bb => f(aa, bb)))

  def map2_2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    (this, b) match {
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
      case (Right(a), Right(b)) => Right(f(a, b))
    }

  def left: E = this match {
    case Left(e) => e
    case _ => sys.error("can't take left from Right")
  }

  def right: A = this match {
    case Right(a) => a
    case _ => sys.error("can't take right from Left")
  }

  def isLeft: Boolean = this match {
    case Left(_) => true
    case _ => false
  }

  def isRight: Boolean = !isLeft
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  // ex 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight(Right(Nil): Either[E, List[A]])(_.map2(_)(_ :: _))

  def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(
      f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil): Either[E, List[B]])((a, acc) =>
      f(a).map2(acc)(_ :: _))
}

