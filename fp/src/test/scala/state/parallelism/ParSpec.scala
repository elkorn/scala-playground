package fp.state.parallelism

import org.scalatest._

import Par._
import parallelism.Par

class ParSpec extends FlatSpec with Matchers {
  val defaultExecService = java.util.concurrent.Executors.newFixedThreadPool(2)
  // "sum" should "sum a sequence of integers" in {
  //   sum(IndexedSeq(1, 2, 3, 4, 5)) should equal(Par.unit(1 + 2 + 3 + 4 + 5))

  // }

  // "map2" should "map two parallel computations together" in {
  //   map2(unit(123), unit(44))(_ + _) should equal(unit(123 + 44))
  // }

  "map" should "uphold the basic mapping laws" in {
    (Par.equal(defaultExecService)(map(unit(1))(_ + 1), unit(2))) should equal(true)

    def id[A](a: A) = a

    def mustBeEqual[A](p1: Par[A], p2: Par[A]) =
      (Par.equal(defaultExecService)(p1, p2)) should equal(true)

    def law1[A](x: A)(f: A => A) =
      mustBeEqual(map(unit(x))(f), unit(f(x)))

    def law2[A](x: A) =
      mustBeEqual(map(unit(x))(id), unit(id(x)))

    def law3[A](x: A) =
      mustBeEqual(map(unit(x))(id), unit(x))

    // The final form of the law: map(y)(id) == y
  }
}
