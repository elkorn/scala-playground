package fp.state.parallelism

import org.scalatest._

import Par._
import parallelism.Par
import scala.util.Failure
import scala.util.Success

class ParSpec extends FlatSpec with Matchers {
  implicit val defaultExecService = java.util.concurrent.Executors.newFixedThreadPool(1)

  "map2" should "map two parallel computations together" in {
    Par.equal(map2(unit(123), unit(44))(_ + _), unit(123 + 44)) should equal(true)
  }

  "run" should "run a Par" in {
    Par.run(unit(1)) should equal(Success(1))
    Par.run(unit(2)) should equal(Success(2))
  }

  "equal" should "test whether the results of two Pars are equal" in {
    Par.equal(unit(1), unit(1)) should equal(true)
    Par.equal(unit(1), unit(2)) should equal(false)
  }

  "map" should "apply a mapping fn to the result of a Par" in {
    val mapped = Par.map(unit(1))(_ + 1)
    Par.run(mapped) should equal(Success(2))
  }

  "map" should "uphold the basic mapping laws" in {
    def id[A](a: A) = a
    true should equal(true)

    def mustBeEqual[A](p1: Par[A], p2: Par[A]) =
      Par.equal(p1, p2) should equal(true)

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
    /*
     To test this properly:
        Inject an ExecutorService which uses a custom ThreadFactory for creating the thread pool. Within that factory, trace the thread instantiations.

        ThreadFactory mock = new CustomObservableThreadFactory();
        ExecutorService executorService = Executors.newCachedThreadPool(mock);
     */
    Par.run(lazyUnit(12)) should equal(Success(12))
  }

  "Par" should "handle exceptions correctly" in {
    val error = new RuntimeException("Boo")
    val expectedFailure = Failure(error)
    def verifyCorrectFailure[A](pa: Par[A]) =
      Par.run(pa).toString() should equal(expectedFailure.toString())

    verifyCorrectFailure(unit(throw error))
    verifyCorrectFailure(map2(unit(12), unit[Int](throw error))(_ + _))
    verifyCorrectFailure(map(unit[Int](throw error))(_ + 1))
  }

  "choice" should "behave like an if statement" in {
    Par.run(choice(unit(true))(unit(1), unit(2))) should equal(Success(1))
    Par.run(choice(unit(false))(unit(1), unit(2))) should equal(Success(2))
    val error = new RuntimeException("Boo")
    val expectedFailure = Failure(error)
    Par.run(choice(unit(false))(unit(1), unit(throw error))).toString() should equal(expectedFailure.toString())
  }

  "choiceN" should "choose a computation from a list based on a previous result" in {
    val list = List(unit("a"), unit("b"), unit("c"), unit("d"))
    def verify(n: Int) =
      Par.equal(choiceN(unit(n))(list), list(n)) should equal(true)

    (0 to 3) foreach verify
  }

  "choiceViaChoiceN" should "behave the same as choice" in {
    Par.equal(choice(unit(true))(unit(1), unit(2)), choiceViaChoiceN(unit(true))(unit(1), unit(2))) should equal(true)
    Par.equal(choice(unit(false))(unit(1), unit(2)), choiceViaChoiceN(unit(false))(unit(1), unit(2))) should equal(true)
    val error = new RuntimeException("Boo")
    val expectedFailure = Failure(error)
    Par.equal(choice(unit(false))(unit(throw error), unit(2)), choiceViaChoiceN(unit(false))(unit(throw error), unit(2))) should equal(true)
  }

  "choiceMap" should "select a Par from a Map based on a key" in {
    val list = List(unit("a"), unit("b"), unit("c"), unit("d"))
    val map = list.zipWithIndex.map {
      case (a, i) => i -> a
    }.toMap

    def verify(n: Int) =
      Par.equal(choiceN(unit(n))(list), choiceMap(unit(n))(map)) should equal(true)

    (0 to 3) foreach verify

  }

  "flatMap" should "choose from anything - working like flatMap" in {
    val list = List(unit("a"), unit("b"), unit("c"), unit("d"))
    val keys = List("a", "b", "c", "d")
    val map = list.zipWithIndex.map {
      case (a, i) => keys(i) -> a
    }.toMap

    def verify(n: Int) =
      Par.equal(flatMap(unit(n))(list), flatMap(unit(keys(n)))(map)) should equal(true)

    (0 to 3) foreach verify
  }
}
