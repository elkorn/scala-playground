
object parallelism {

  // What should go here?
  type Par[A] = A

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
      inputs.headOption.getOrElse(0)
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
     * Takes a computation and returns the description of its async evaluation.
     * It's a *derived* combinator, meaning that it's expressed solely in terms of other combinators.
     * @param computation an unevaluated A
     * @tparam A computation result type
     * @return a computation that might be evaluated in a separate thread
     */
    def lazyUnit[A](computation: => A): Par[A] = fork(unit(computation))

    // Gives parallelism control to the programmer.
    // Problems solved:
    // - instantiating parallelism too strictly,
    // - not being able to choose whether a task should be performed async.
    // If `fork` was to run the provided computation immediately, its implementation would require knowledge about
    // parallelization resources (e.g. have a global thread pool).
    // This is unwanted here, we want to give more fine-grained control to the user.
    def fork[A](a: => Par[A]): Par[A] = a

    /**
     * @param parallelComputation a parallel computation
     * @tparam A computation result typeA
     * @return resulting value extracted from a parallel computation.
     */
    def get[A](parallelComputation: Par[A]): A = parallelComputation

    // Using strict arguments here would cause the whole left side of the evaluation tree to be built before
    // proceeding to the right side.
    // Supplying arguments as non-strict allows for fair parallel execution of both units.
    // The downside here is that the API is implicit about when the computations are forked off the main thread.
    def map2[A](a1: => Par[A], a2: => Par[A])(f: (Par[A], Par[A]) => A): Par[A] =
      Par.unit(f(a1, a2))

    // Having defined the `fork` function, we can use strict arguments here, leaving the parallelism up to the
    // user of the library.
    def strictMap2[A](a1: Par[A], a2: Par[A])(f: (Par[A], Par[A]) => A): Par[A] =
      Par.unit(f(a1, a2))

    /**
     * Takes a computation and returns the description of its async evaluation.
     * @param computation an evaluated A
     * @tparam A computation result type
     * @return a computation that is being evaluated in a seprate thread.
     */
    def unit[A](computation: A): Par[A] = computation
  }

  parSum2(IndexedSeq(1, 2, 3, 5))
}