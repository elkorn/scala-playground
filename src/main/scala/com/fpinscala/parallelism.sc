import java.util.concurrent
import java.util.concurrent.{Callable, ExecutorService}

import scala.concurrent.duration._
import scala.concurrent.{Await, CanAwait, ExecutionContext, Future}
import scala.util.{Success, Try}

object parallelism {
  type Par[A] = ExecutorService => Future[A]

  def nonParallelizableSum(ints: List[Int]): Int =
    ints.foldLeft(0)(_ + _)

  def parSum0(inputs: IndexedSeq[Int]): Int =
    if (inputs.size <= 1)
      inputs.headOption.getOrElse(0)
    else {
      val (l, r) = MyIdea.inHalf(inputs)
      parSum0(l) + parSum0(r)
    }

  def parSum1(inputs: IndexedSeq[Int]): Int =
    if (inputs.size <= 1)
      inputs.headOption.getOrElse(0)
    else {
      val (l, r) = MyIdea.inHalf(inputs)
      // Simply using Par.get(Par.unit(parSum1(l))) + Par.get(Par.unit(parSum1(r))) would effectively
      // make the computation sequential.
      // The get() will block to wait for the result of unit() immediately after spawning its thread.
      val sumL: Par[Int] = Par.unit(parSum1(l))
      val sumR: Par[Int] = Par.unit(parSum1(r))
      Par.get(sumL) + Par.get(sumR)
      // This highlights that unit() has a side effect *with regard to* get().
      // Until we use get(), it just returns a Par which can be composed etc.
    }

  def parSum2(inputs: IndexedSeq[Int]): Par[Int] =
    if (inputs.size <= 1)
      Par(Par.UnitFuture(inputs.headOption.getOrElse(0)))
    else {
      val (l, r) = MyIdea.inHalf(inputs)
      // Inexplicit forking:
      // Par.map2(parSum2(l), parSum2(r))(_ + _)
      // Explicit forking:
      Par.strictMap2(Par.fork(parSum2(l)), Par.fork(parSum2(r)))(_ + _)
    }

  object MyIdea {
    def divideAndConquer[A](z: A, op: (A, A) => A)(inputs: IndexedSeq[A]): A = {
      if (inputs.size <= 1)
        inputs.headOption.getOrElse(z)
      else {
        val (l, r) = inputs.splitAt(inputs.length / 2)
        // + can be generalized
        op(divideAndConquer(z, op)(l), divideAndConquer(z, op)(l))
      }
    }

    def parSum2(inputs: IndexedSeq[Int]): Int =
      divideAndConquer[Int](0, (a, b) => a + b)(inputs)

    def inHalf(inputs: IndexedSeq[Int]): (IndexedSeq[Int], IndexedSeq[Int]) = {
      inputs.splitAt(inputs.length / 2)
    }
  }

  object Par {

    /**
     * @param parallelComputation a parallel computation
     * @tparam A computation result typeA
     * @return resulting value extracted from a parallel computation.
     */
    def get[A](parallelComputation: Par[A]): A = ???

    // Map2 functions combine the results of two concurrent computations.
    // Having defined the `fork` function, we can use strict arguments here, leaving the parallelism up to the
    // user of the library.
    def strictMap2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      (s: ExecutorService) => {
        val atMost = 10 seconds
        val af = a(s)
        val bf = b(s)
        UnitFuture(f(Await.result(af, atMost), Await.result(bf, atMost)))
      }

    /**
     * Takes a computation and returns the description of its async evaluation.
     * It's a *derived* combinator, meaning that it's expressed solely in terms of other combinators.
     * Wraps the computation for concurrent evaluation by run().
     * @param computation an unevaluated A
     * @tparam A computation result type
     * @return a computation that might be evaluated in a separate thread
     */
    def lazyUnit[A](computation: => A): Par[A] = fork(unit(computation))

    /**
     * Takes a computation and returns the description of its async evaluation.
     * @param value an evaluated A
     * @tparam A computation result type
     * @return a computation that is being evaluated in a separate thread.
     */
    def unit[A](value: A): Par[A] = (s: ExecutorService) => UnitFuture(value)

    /**
     * Gives parallelism control to the programmer.
     * Problems solved:
     * - instantiating parallelism too strictly,
     * - not being able to choose whether a task should be performed async.
     * If `fork` was to run the provided computation immediately, its implementation would require knowledge about
     * parallelization resources (e.g. have a global thread pool).
     * This is unwanted here, we want to give more fine-grained control to the user.
     * The assumed meaning of forking is to take an unresolved Par and "mark" it for concurrent evaluation.
     * @param computation a computation to be concurrently evaluated.
     * @tparam A computation result type
     * @return a computation marked for concurrent evaluation
     */
    def fork[A](computation: => Par[A]): Par[A] =
      executorService => {
        val callable = new Callable[A] {
          def call = Await.result(computation(executorService), 10 seconds)
        }

        val submit: concurrent.Future[A] = executorService.submit(callable)
        val fut = submit
        fut.asInstanceOf[Future[A]]
      }

    /**
     * Extracts a value from a Par by actually performing the computation.
     * @param computation the computation to perform
    @tparam A computation result type
     * @return result of the computation
     */
    def run[A](s: ExecutorService)(computation: Par[A]): Future[A] = computation(s)

    // Having defined fork as such, Par does not need to know how to actually implement the parallelism.
    // It's more of a description left for later interpretation.
    // Par becomes more of a first-class program to be run instead of just a container for a value to be
    // extracted.
    // Run provides means of implementing the parallelism

    def apply[A](f: => Future[A]): Par[A] =
      (x: ExecutorService) => f

    case class UnitFuture[A](get: A) extends Future[A] {
      override def result(atMost: Duration)(implicit permit: CanAwait): A = get

      override def onComplete[U](f: (Try[A]) => U)(implicit executor: ExecutionContext): Unit = f(Success(get))

      override def isCompleted: Boolean = true

      override def value: Option[Try[A]] = Some(Success(get))

      override def ready(atMost: Duration)(implicit permit: CanAwait): this.type = this

      //      Error:(145, 21) overriding method ready in trait Awaitable of type (atMost: scala.concurrent.duration.Duration)(implicit permit: scala.concurrent.CanAwait)UnitFuture.this.type;
      //      method ready has incompatible type
    }
  }

  parSum2(IndexedSeq(1, 2, 3, 5))
}
