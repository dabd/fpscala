package fpinscala.parallelism

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {

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
  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))
}
