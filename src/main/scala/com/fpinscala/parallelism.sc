import java.util.concurrent.{Callable, ExecutorService, Executors}

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Success, Try}
object parallelism {
  type Par[A] = ExecutorService => Future[A]
  private val pool: ExecutorService = Executors.newScheduledThreadPool(6)
  def nonParallelizableSum(ints: List[Int]): Int =
    ints.foldLeft(0)(_ + _)
  //
  //  def parSum1(inputs: IndexedSeq[Int]): Int =
  //    if (inputs.size <= 1)
  //      inputs.headOption.getOrElse(0)
  //    else {
  //      val (l, r) = MyIdea.inHalf(inputs)
  //      // Simply using Par.get(Par.unit(parSum1(l))) + Par.get(Par.unit(parSum1(r))) would effectively
  //      // make the computation sequential.
  //      // The get() will block to wait for the result of unit() immediately after spawning its thread.
  //      val sumL: Par[Int] = Par.unit(parSum1(l))
  //      val sumR: Par[Int] = Par.unit(parSum1(r))
  //      Par.get(sumL) + Par.get(sumR)
  //      // This highlights that unit() has a side effect *with regard to* get().
  //      // Until we use get(), it just returns a Par which can be composed etc.
  //    }
  def parSum0(inputs: IndexedSeq[Int]): Int =
    if (inputs.size <= 1)
      inputs.headOption.getOrElse(0)
    else {
      val (l, r) = MyIdea.inHalf(inputs)
      parSum0(l) + parSum0(r)
    }
  def parSum2(inputs: IndexedSeq[Int]): Par[Int] = {
    println(inputs, inputs.size)
    if (inputs.size <= 1) {
      println("returning unitFuture")
      Par(Par.UnitFuture(inputs.headOption.getOrElse(0)))
    }
    else {
      val (l, r) = MyIdea.inHalf(inputs)
      println("split")
      println(l, r)
      // Inexplicit forking:
      // Par.map2(parSum2(l), parSum2(r))(_ + _)
      // Explicit forking:
      Par.map2(Par.fork(parSum2(l)), Par.fork(parSum2(r)))(_ + _)
    }
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

    def inHalf[A](inputs: IndexedSeq[A]): (IndexedSeq[A], IndexedSeq[A]) =
      inputs.splitAt(inputs.length / 2)
  }
  object Par {

    /**
     * @param parallelComputation a parallel computation
     * @tparam A computation result typeA
     * @return resulting value extracted from a parallel computation.
     */
    def get[A](s: ExecutorService)(parallelComputation: Par[A]): A = {
      Await.result(Par.run(s)(parallelComputation), 20 seconds)
    }

    /**
     * Extracts a value from a Par by actually performing the computation.
     * @param computation the computation to perform
    @tparam A computation result type
     * @return result of the computation
     */
    def run[A](s: ExecutorService)(computation: Par[A]): Future[A] = computation(s)

    def apply[A](f: => Future[A]): Par[A] =
      (x: ExecutorService) => f

    def sort(parList: Par[List[Int]]): Par[List[Int]] =
      map(parList)(_.sorted)

    def filter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val p = as.map(asyncF((a) => if (f(a)) List(a) else Nil))
      map(sequence(p))(_.flatten)
    }

    def parTransform[A,B,C](ps: List[A])(mapF: A=>B)(reduceF:List[B]=>C): Par[C] = {
      map(parMap(ps)(mapF))(reduceF)
    }

    // Having defined fork as such, Par does not need to know how to actually implement the parallelism.
    // It's more of a description left for later interpretation.
    // Par becomes more of a first-class program to be run instead of just a container for a value to be
    // extracted.
    // Run provides means of implementing the parallelism
    // Thanks to the top-level fork, this will create one async computation responsible
    // for spawning all the parallel computations from within map/asyncF and then wait for them to finish.
    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      // Need something to collect the results...
      sequence(fbs)
    }

    def asyncF[A, B](f: A => B): A => Par[B] = (a) => lazyUnit(f(a))

    //    def sort[A](parList: Par[List[A]]): Par[List[A]] =
    //      map2(parList, unit())((a, _) => a.sorted)

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

        import scala.concurrent.ExecutionContext.Implicits.global

        Future {
          executorService.submit(callable).get
        }
      }

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      ps.foldRight(unit(List(): List[A]))((h, t) => map2(h, t)(_ :: _))
    }

    //    def map3[A,B,C,D](a: Par[A],b:Par[B],c:Par[C])(f: (A,B,C)=>D): Par[D] =
    //      map2[A,B,D](a,b) { (a,b) =>
    //        map2[C,Unit,D](c, unit()) { (c,_) =>
    //          f(a,b,c)
    //        }
    //      }

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit())((a, _) => f(a))

    // Map2 functions combine the results of two concurrent computations.
    // Having defined the `fork` function, we can use strict arguments here, leaving the parallelism up to the
    // user of the library.
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      (s: ExecutorService) => {
        val bf = b(s)
        val af = a(s)
        //        val atMost = 10 seconds
        //        This representation does not honor timeouts internally.
        //        UnitFuture(f(Await.result(af, atMost), Await.result(bf, atMost)))
        Map2Future(af, bf)(f)
      }

    /**
     * Takes a computation and returns the description of its async evaluation.
     * @param value an evaluated A
     * @tparam A computation result type
     * @return a computation that is being evaluated in a separate thread.
     */
    def unit[A](value: A): Par[A] = (s: ExecutorService) => UnitFuture(value)

    def sumWords(ps: List[String]): Par[Int] = {
      map(parMap(ps)(p => p.split(" ").length))(_.sum)
    }

    case class UnitFuture[A](get: A) extends Future[A] {
      override def result(atMost: Duration)(implicit permit: CanAwait): A = get

      override def onComplete[U](f: (Try[A]) => U)(implicit executor: ExecutionContext): Unit = f(Success(get))

      override def isCompleted: Boolean = true

      override def value: Option[Try[A]] = Some(Success(get))

      override def ready(atMost: Duration)(implicit permit: CanAwait): this.type = this
    }

    case class Map2Future[A, B, C](a: Future[A], b: Future[B])(f: (A, B) => C) extends Future[C] {
      private var cache: Option[C] = None

      override def onComplete[U](fu: (Try[C]) => U)(implicit executor: ExecutionContext): Unit = {
        (for {
          va <- a
          vb <- b
        } yield (va, vb))
          .map((p) => f(p._1, p._2))
          .onComplete(fu)
      }

      override def isCompleted: Boolean = a.isCompleted && b.isCompleted

      override def value: Option[Try[C]] = Some(Success(compute(2 seconds)))

      private def compute(atMost: Duration): C = cache match {
        case Some(c) => c
        case None => {
          val start = System.currentTimeMillis()
          val va = Await.result(a, atMost)
          val next = System.currentTimeMillis()
          val vb = Await.result(b, (next - start) millis)
          cache = Some(f(va, vb))
          cache.get
        }
      }

      @scala.throws[Exception](classOf[Exception])
      override def result(atMost: Duration)(implicit permit: CanAwait): C = compute(atMost)

      @scala.throws[InterruptedException](classOf[InterruptedException])
      @scala.throws[TimeoutException](classOf[TimeoutException])
      override def ready(atMost: Duration)(implicit permit: CanAwait): this.type = this
    }

  }

  //  Par.get(pool)(Par.map2(Par.unit(12), Par.unit(7))(_ + _))
  //  Par.get(pool)(Par.asyncF[Int, Int](_ + 2)(3))
  //  Par.get(pool)(parSum2(IndexedSeq(1, 2, 3, 4)))
  //  Par.get(pool)(parSum2(IndexedSeq(1, 2, 3, 4, 5, 6)))
  //  Par.get(pool)(Par.parMap(List(1, 2, 3, 4, 5))(_ + 3))
  //  Par.get(pool)(Par.filter(List(1, 2, 3, 4, 5))(_ % 2 == 0))
  Par.get(pool)(Par.sumWords(List("Lorem ipsum dolor sit amet, consectetur adipiscing elit,", "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.", "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.")))
  object Tests {
    val timeout = 10 seconds
    def equals[A](es: ExecutorService)(a1: Par[A], a2: Par[A]): Boolean = {
      Await.result(Par.run(es)(a1), timeout) == Await.result(Par.run(es)(a2), timeout)
    }
  }
  Tests.equals(Executors.newFixedThreadPool(2))(Par.map(Par.unit(1))(_ + 1), Par.unit(2))
}

