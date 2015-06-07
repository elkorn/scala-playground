package fp.state.parallelism

import fp.property.Prop
import fp.property.SizedGen
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.ThreadFactory
import org.scalatest._

import Par._
import parallelism.Par
import scala.util.Failure
import scala.util.Success
import fp.property.Gen

class ParSpec extends FlatSpec with Matchers {
  implicit val defaultExecService = java.util.concurrent.Executors.newFixedThreadPool(1)
  val S = Gen.weighted(
    Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25
  )

  // TODO: Implement this for sized generators as well.
  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    Gen.forAll(S ** g) {
      case (s, a) => Par.run(f(a))(s).get
    }

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

    // Creates a fixed thread pool exec. 75% of the time and an unbounded one 25% of the time.
    import parallelism._
    import fp.state.parallelism.Future

    def checkPar[A](p: Par[Boolean]): Prop =
      forAllPar(Gen.unit(()))(_ => p)

    def mustBeEqual[A](p1: Par[A], p2: Par[A]) =
      Par.equal(p1, p2) should equal(true)

    def law1[A](x: A)(f: A => A) =
      mustBeEqual(map(unit(x))(f), unit(f(x)))

    val law1prop = checkPar {
      areEqual(
        map(unit(1))(_ + 1),
        unit(2)
      )
    }

    def law2[A](x: A) =
      mustBeEqual(map(unit(x))(id), unit(id(x)))

    def law3[A](x: A) =
      mustBeEqual(map(unit(x))(id), unit(x))

    // The final form of the law: map(y)(id) == y
    val law3prop = forAllPar(Gen.choose(0, 10).map(unit(_))) { n =>
      areEqual(map(n)(y => y), n)
    }

    (1 to 10) foreach (x => {
      law1(x)(_ + 123)
      law2(x)
      law3(x)
    })

    Prop.check(law1prop) should equal(Gen.Result.Proven)
    Prop.check(law3prop) should equal(Gen.Result.Proven)
  }

  "writing more complicated par opertaions" should "be possible with generators" in {
    // Don't really know what to do with this.
    // What it does is creates lists of 0..20 numbers in <-100, 100), spawns a separate thread for adding each number to a result accumulator.
    val complicated: Gen[Par[Int]] = Gen.choose(-100, 100).listOfN(Gen.choose(0, 20)).map(_.foldLeft(Par.unit(0))((p, i) => fork { map2(p, unit(i))(_ + _) }))

    // TODO: Bug in cartesian?
    // Prop.check(Gen.forAll(complicated) { sumPar =>
    //   {
    //     val sumTry = Par.run(sumPar)
    //     sumTry match {
    //       case Success(sum) => sum > -2000 && sum < 2000
    //       case Failure(err) => false
    //     }
    //   }
    // }) should equal(Gen.Result.Unfalsified)
  }

  "fork" should "spawn a computation on a separate thread" in {
    class CustomObservableThreadFactory() extends ThreadFactory() {
      private var counter = 0

      def spawnedThreads = counter
      override def newThread(r: Runnable): Thread = {
        counter += 1
        new Thread(r, s"CustomObservableThreadFactory-$counter")
      }
    }

    val mock = new CustomObservableThreadFactory()
    val executor = Executors.newSingleThreadExecutor(mock)

    Par.run(unit(12))(executor)
    mock.spawnedThreads should equal(0)
    Par.run(fork(unit(12)))(executor)
    mock.spawnedThreads should equal(1)
  }

  "fork" should "obey the algebraic law" in {
    Prop.check(
      forAllPar(Gen.choose(-1000, 1000)) { x =>
        map(lazyUnit(x))(_ == x)
      }
    ) should equal(Gen.Result.Proven)
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

  "join" should "flatten a nested Par" in {
    Par.run(join(unit(unit(1)))) should equal(Success(1))
  }

  "joinViaFlatMap" should "work the same as join" in {
    Par.equal(joinViaFlatMap(unit(unit(1))), join(unit(unit(1)))) should equal(true)
  }

  "flatMapViaJoin" should "work the same as flatMap" in {
    val list = List(unit("a"), unit("b"), unit("c"), unit("d"))

    def verify(n: Int) =
      Par.equal(flatMapViaJoin(unit(n))(list), flatMap(unit(n))(list)) should equal(true)

    (0 to 3) foreach verify
  }

  "map2_fmu" should "work the same as map2" in {
    Par.equal(map2_fmu(unit(123), unit(44))(_ + _), map2(unit(123), unit(44))(_ + _)) should equal(true)
  }
}
