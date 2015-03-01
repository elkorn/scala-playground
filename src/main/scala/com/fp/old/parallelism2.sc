import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}

import com.fp.Actor

import scala.util.{Failure, Success, Try}

object parallelism2 {

  type Par[A] = ExecutorService => Future[A]
  val x = Par.run(Executors.newFixedThreadPool(1))(Par.map2(Par.unit({
    throw new Error("test1")
    1
  }), Par.unit(2))(_ + _))

  sealed trait Future[A] {
    // In normal code (not a worksheet) this would be made package private so that the library user does not
    // have to deal with it.
    // The callback employs a technique of using side effects as an implementation detail for a
    // purely functional API.
    // These side effects are local only - they cannot be observed by outside code.
    // This is acceptable in pure FP.
    /*private[parallelism2] */ def apply(callback: A => Unit): Unit
  }

  Par.run(Executors.newFixedThreadPool(1))(Par.map2(Par.unit(1), Par.unit(2))(_ + _))

  object Par {
    def run[A](es: ExecutorService)(p: Par[A]): Try[A] = {
      // Thread-safe mutable reference for storing the result.
      val ref = new AtomicReference[Try[A]]
      // Similar to WaitGroup / semaphore
      val latch = new CountDownLatch(1)
      try {
        p(es) { a =>
          ref.set(Success(a))
          latch.countDown()
        }
      } catch {
        case any => ref.set(Failure(any))
          latch.countDown()
      }

      latch.await
      ref.get
      // run() will block here until countDown is called 1 time.
    }

    def unit[A](a: A): Par[A] = {
      es => new Future[A] {
        def apply(callback: A => Unit): Unit = {
          callback(a)
        }
      }
    }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        override /*private[parallelism2]*/ def apply(callback: (A) => Unit): Unit =
        // Forks off evaluation of `a` and returns immediately.
        // The result of `a` will be handled by `callback`.
          evaluate(es)(a(es)(callback))

      }

    // Evaluates an action asynchronously.
    private def evaluate(es: ExecutorService)(action: => Unit): Unit = {
      es.submit(new Callable[Unit] {
        def call = action
      })
    }

    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        override /*private[parallelism2]*/ def apply(callback: (C) => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None

          val combiner = Actor[Either[A, B]](es)({
            case Left(a) => br match {
              case None => ar = Some(a)
              case Some(b) => evaluate(es)(callback(f(a, b)))
            }

            case Right(b) => ar match {
              case None => br = Some(b)
              case Some(a) => evaluate(es)(callback(f(a, b)))
            }
          }, {
            case err => println("an err!", err)
          })

          try {
            pa(es)(a => combiner ! Left(a))
            pb(es)(b => combiner ! Right(b))
          } catch {
            case _ => println("err")
          }
        }
      }
  }

  println("caught error")
  x
}
