package fp.state.parallelism

import org.scalatest._

import Par._
import parallelism.Par

class ParSpec extends FlatSpec with Matchers {
  val defaultExecService = java.util.concurrent.Executors.newFixedThreadPool(3)
  // "sum" should "sum a sequence of integers" in {
  //   sum(IndexedSeq(1, 2, 3, 4, 5)) should equal(Par.unit(1 + 2 + 3 + 4 + 5))

  // }

  // "map2" should "map two parallel computations together" in {
  //   map2(unit(123), unit(44))(_ + _) should equal(unit(123 + 44))
  // }

  "run" should "run a Par" in {
    Par.run(defaultExecService)(unit(1)) should equal(1)
    Par.run(defaultExecService)(unit(2)) should equal(2)
  }

  "equal" should "test whether the results of two Pars are equal" in {
    Par.equal(defaultExecService)(unit(1), unit(1)) should equal(true)
    Par.equal(defaultExecService)(unit(1), unit(2)) should equal(false)
  }

  "map" should "apply a mapping fn to the result of a Par" in {
    val mapped = Par.map(unit(1))(_ + 1)
    Par.run(defaultExecService)(mapped) should equal(2)
  }

  "map" should "uphold the basic mapping laws" in {
    def id[A](a: A) = a
    true should equal(true)

    def mustBeEqual[A](p1: Par[A], p2: Par[A]) =
      (Par.equal(defaultExecService)(p1, p2)) should equal(true)

    def law1[A](x: A)(f: A => A) =
      mustBeEqual(map(unit(x))(f), unit(f(x)))

    def law2[A](x: A) =
      mustBeEqual(map(unit(x))(id), unit(id(x)))

    def law3[A](x: A) =
      mustBeEqual(map(unit(x))(id), unit(x))

    // The final form of the law: map(y)(id) == y

    (1 to 10) foreach (x => {
      law1(x)(_ + 123)
      law2(x)
      law3(x)
    })
  }

  "fork" should "spawn a computation on a separate thread" in {
    // How to test this properly?
    Par.run(defaultExecService)(lazyUnit(12)) should equal(12)
  }
}
