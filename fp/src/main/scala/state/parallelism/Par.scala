package fp.state.parallelism

import java.util.concurrent.{ Callable, CountDownLatch, ExecutorService }
import java.util.concurrent.atomic.AtomicReference

package object parallelism {
  type Par[A] = ExecutorService => Future[A]
}

sealed trait Future[A] {
  private[parallelism] def apply(k: A => Unit): Unit
}

object Par {
  import parallelism._
  import fpinscala.parallelism._

  def run[A](es: ExecutorService)(pa: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)

    pa(es) { a =>
      ref.set(a)
      val previous = latch.getCount()
      latch.countDown
    }

    latch.await
    ref.get
  }

  def unit[A](a: A): Par[A] =
    ex => new Future[A] {
      def apply(cb: A => Unit): Unit = cb(a)
    }
  // A derived combinator - one that is expressed wholly in terms of other operations.
  // It does not need to know anything about the representation of Par.
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  /*
   The function f does is not evaluated on a separate logical thread.
   To achieve that, one might use fork(map2(a,b)(f)).
   */
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      def apply(cb: C => Unit): Unit = {
        def process(a: A, b: B): Unit =
          eval(es)(cb(f(a, b)))

        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = Actor[Either[A, B]](es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => process(a, b)
          }

          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => process(a, b)
          }
        }

        pa(es)(a => combiner ! Left(a))
        pb(es)(b => combiner ! Right(b))
      }
    }

  def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
    run(e)(p1) == run(e)(p2)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    (es: ExecutorService) => new Future[B] {
      def apply(cb: B => Unit): Unit = {
        pa(es)(a => eval(es)(cb(f(a))))
      }
    }

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit = {
        eval(es)(a(es)(cb))
      }
    }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })
}
