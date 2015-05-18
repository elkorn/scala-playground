package fp.state.parallelism

import java.util.concurrent.{ Future, ExecutorService, Callable, TimeUnit }
import scala.concurrent.duration._

package object parallelism {
  type Par[A] = ExecutorService => Future[A]
}

object Par {
  import parallelism._

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
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

  def unit[A](a: A): Par[A] = es => UnitFuture(a)
  // A derived combinator - one that is expressed wholly in terms of other operations.
  // It does not need to know anything about the representation of Par.
  def lazyUnit[A](a: => A) = ??? // fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  /*
   Allows the user of the library to specify explicitly that a
   computation should be run on a separate logical thread.
   map2 can be made strict, since the decision of forking is left to the user.
   */
  // def fork[A](a: => Par[A]): Par[A] =
  //   es => es.submit(new Callable[A] {
  //     def call = a(es).get
  //   })

  /*
   Extracts the value from a computation by performing it.
   Returning a Future gives more control to the user by leaving timeouts, cancellation etc. up to her.
   */
  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  // def sum(ints: IndexedSeq[Int]): Par[Int] = {
  //   if (ints.length <= 1) unit(ints.headOption.getOrElse(0))
  //   else {
  //     val (l, r) = ints.splitAt(ints.length / 2)
  //     map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
  //   }
  // }

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

}
