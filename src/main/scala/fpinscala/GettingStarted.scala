package fpinscala

import scala.math.BigInt

object GettingStarted {

  /**
    * ex 2.1
    * fibonacci using dynamic programming:
    * fib(0) = 0, fib(1) = 1
    * fib(i) = fib(i-2) + fib(i-1), for 2 <= i <= n
    *
    * @param n
    * @return nth fibonacci number
    */
  def fib(n: BigInt): BigInt = {
    @annotation.tailrec
    def go(i: BigInt, p: BigInt, c: BigInt): BigInt = if (i < n) go(i + 1, c, p + c) else p + c

    val zero = BigInt(0)
    val one = BigInt(1)
    n match {
      case `zero` => 0
      case `one` => 1
      case _ => go(2, 0, 1)
    }
  }

  // ex 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as.sliding(2).forall(xs => xs match {
      case Array(x, y, _*) => ordered(x, y)
      case _ => true
    })
  }

  // ex 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // ex 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // ex 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

}
