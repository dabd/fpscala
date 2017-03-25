package fpinscala.parallelism

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Blocking {

  // ex 7.1
  //  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // ex 7.4
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def lift[A, B](f: A => B): Par[A] => Par[B] =
    pa => map(pa)(f)

  // ex 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // ex 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val xs: List[Par[List[A]]] =
      as.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(xs))(_.flatten)
  }

  // p. 110
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Blocking.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Blocking.map2(Blocking.fork(sum(l)), Blocking.fork(sum(r)))(_ + _)
    }

  // a parallel fold only works if bs forms a monoid under f (meaning f is associative and z is the neutral element)
  // a reduce only requires bs forms a semigroup under f (f is associative)
  def reduce[A](xs: IndexedSeq[A], z: A)(f: (A, A) => A): Par[A] = {
    def rec(ys: IndexedSeq[A]): Par[A] =
      ys match {
        case Seq(y) => unit(y)
        case _ =>
          val (l, r) = ys.splitAt(ys.length / 2)
          map2(fork(rec(l)), fork(rec(r)))(f)
      }
    xs match {
      case Seq() => unit(z)
      case Seq(x) => unit(f(x, z))
      case ws => map2(unit(z), rec(ws))(f)
    }
  }

  def max(x: Option[Int], y: Option[Int]): Option[Int] =
    for {
      x <- x
      y <- y
    } yield scala.math.max(x, y)

  def maximum(xs: IndexedSeq[Int]): Option[Par[Int]] =
    xs match {
      case Seq() => None
      case Seq(h, t @ _ *) =>
        Some(reduce(t.toIndexedSeq, h)((x, y) => scala.math.max(x, y)))
    }

  def countWords(paragraphs: List[String]): Par[Int] =
    (es: ExecutorService) => {
      val counts = parMap(paragraphs)(_.split("\\W+").length)(es).get()
      reduce(counts.toIndexedSeq, 0)(_ + _)(es)
    }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(
      f: (A, B, C) => D): Par[D] =
    (es: ExecutorService) =>
      map2(b, c)((b2, c2) => (f.curried)(a(es).get())(b2)(c2))(es)

  def map3_v2[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(
      f: (A, B, C) => D): Par[D] =
    map2(map2(a, b)((x, y) => (x, y)), c) {
      case ((a1, b1), c1) => f(a1, b1, c1)
    }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(
      f: (A, B, C, D) => E): Par[E] =
    map2(map2(a, b)((x, y) => (x, y)), map2(c, d)((x, y) => (x, y))) {
      case ((a1, b1), (c1, d1)) => f(a1, b1, c1, d1)
    }

  def map5[A, B, C, D, E, F](a: Par[A],
                             b: Par[B],
                             c: Par[C],
                             d: Par[D],
                             e: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
    map2(map2(map2(a, b)((x, y) => (x, y)), c)((x, y) => (x, y)),
         map2(d, e)((x, y) => (x, y))) {
      case (((a1, b1), c1), (d1, e1)) => f(a1, b1, c1, d1, e1)
    }

}
