package fp.state.parallelism

import java.util.concurrent.{ ExecutorService, Callable, TimeUnit, CountDownLatch }
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration._

package object parallelism {
  type Par[A] = ExecutorService => Future[A]
}

sealed trait Future[A] {
  private[parallelism] def apply(k: A => Unit): Unit
}

object Par {
  import parallelism._

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = apply(a => ())
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class Map2Future[A, B, C](fa: Future[A], fb: Future[B], f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    def isDone = cache.isDefined
    def isCancelled = fa.isCancelled || fb.isCancelled
    def cancel(evenIfRunning: Boolean) =
      fa.cancel(evenIfRunning) || fa.cancel(evenIfRunning)
    def get = compute(Long.MaxValue)
    def get(timeout: Long, unit: TimeUnit): C =
      compute(TimeUnit.MILLISECONDS.convert(timeout, unit))

    private def compute(timeoutMs: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.currentTimeMillis
        val ar = fa.get(timeoutMs, TimeUnit.MILLISECONDS)
        val stop = System.currentTimeMillis
        val at = stop - start
        val br = fb.get(timeoutMs - at, TimeUnit.MILLISECONDS)
        val ret = (f(ar, br))
        cache = Some(ret)
        ret
    }
  }

  def run[A](es: ExecutorService)(pa: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)

    pa(es) { a =>
      ref.set(a)
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
  def lazyUnit[A](a: => A) = ??? // fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  /*
   The function f does is not evaluated on a separate logical thread.
   To achieve that, one might use fork(map2(a,b)(f)).
   */
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val af = pa(es)
      val bf = pb(es)
      Map2Future(af, bf, f)
    }

  def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
    run(e)(p1) == run(e)(p2)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    (es: ExecutorService) => UnitFuture(f(run(es)(pa)))

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)
}
