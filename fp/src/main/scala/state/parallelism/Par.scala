package fp.state.parallelism

import java.util.concurrent.{ Callable, CountDownLatch, ExecutorService }
import java.util.concurrent.atomic.AtomicReference
import scala.util.Try
import scala.util.Success

package object parallelism {
  type Par[A] = ExecutorService => Future[A]
  type Callback[A] = Try[A] => Unit
}

sealed trait Future[A] {
  private[parallelism] def apply(k: parallelism.Callback[A]): Unit
}

object Par {
  import parallelism._
  import fpinscala.parallelism._
  import scala.util.Failure

  def run[A](pa: Par[A])(implicit es: ExecutorService): Try[A] = {
    val ref = new AtomicReference[Try[A]]
    val latch = new CountDownLatch(1)

    pa(es) { a =>
      ref.set(a)
      val previous = latch.getCount()
      latch.countDown
    }

    latch.await
    ref.get
  }

  def unit[A](a: => A): Par[A] =
    ex => new Future[A] {
      def apply(cb: Callback[A]): Unit = cb(try {
        Success(a)
      } catch {
        case e: Throwable => Failure(e)
      })
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
      def apply(cb: Callback[C]): Unit = {
        def process(a: A, b: B): Unit =
          eval(es)(cb {
            try {
              Success(f(a, b))
            } catch {
              case e: Throwable => Failure(e)
            }
          })

        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = Actor[Either[Try[A], Try[B]]](es) {
          case Left(Failure(fail)) => cb(Failure(fail))
          case Right(Failure(fail)) => cb(Failure(fail))

          case Left(Success(a)) => br match {
            case None => ar = Some(a)
            case Some(b) => process(a, b)
          }

          case Right(Success(b)) => ar match {
            case None => br = Some(b)
            case Some(a) => process(a, b)
          }
        }

        pa(es)(a => combiner ! Left(a))
        pb(es)(b => combiner ! Right(b))
      }
    }

  def equal[A](p1: Par[A], p2: Par[A])(implicit es: ExecutorService): Boolean =
    run(p1) == run(p2)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    (es: ExecutorService) => new Future[B] {
      def apply(cb: Callback[B]): Unit = {
        pa(es) {
          case Success(a) => eval(es) {
            try {
              cb(Success(f(a)))
            } catch {
              case e: Throwable => cb(Failure(e))
            }
          }

          case Failure(err) => cb(Failure(err))
        }
      }
    }

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: Callback[A]): Unit = {
        eval(es)(a(es)(cb))
      }
    }

  private def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })
}
