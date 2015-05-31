package fp.property

import Gen._
import org.scalatest.{ FlatSpec, Matchers }

class PropertySpec extends FlatSpec with Matchers {
  val gen = fp.state.SimpleRNG(123L)
  def verifyEqual[A](a1: Gen[A], a2: Gen[A]) =
    a1.sample.run(gen) should equal(a2.sample.run(gen))

  "listOfN" should "create a generator with a fixed-sized list" in {
    val list = listOfN(10, Gen.choose(0, 5)).sample.run(gen)._1

    list shouldBe a[List[_]]
    list should have size (10)
    all(list) should be < 5
    all(list) should be >= 0
  }

  "unit" should "return a generator giving a constant value" in {
    unit(12).sample.run(gen) should equal((12, gen))
  }

  "choosePair" should "return a generator of integer pairs" in {
    val list = listOfN(10, Gen.choosePair(0, 5)).sample.run(gen)._1
    def verify(n: Int) =
      {
        n should be < 5
        n should be >= 0
      }

    list shouldBe a[List[_]]
    list should have size 10
    list foreach {
      case (n1, n2) =>
        verify(n1)
        verify(n2)
    }
  }

  "flatMap" should "apply a mapping fn and flatten the result" in {
    verifyEqual(unit(13).flatMap(x => unit(x + 7)), unit(20))
  }

  "listOfN" should "support dynamic sizing" in {
    val list: List[Int] = Gen.choose(0, 5).listOfN(unit(10)).sample.run(gen)._1
    def verify(n: Int) =
      {
        n should be < 5
        n should be >= 0
      }

    list shouldBe a[List[_]]
    list should have size 10
    list foreach verify
  }
}
