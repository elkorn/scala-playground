package fp.property

import Gen._
import org.scalatest.{ FlatSpec, Matchers }

class PropertySpec extends FlatSpec with Matchers {
  val gen = fp.state.SimpleRNG(123L)
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
    list should have size (10)
    list foreach {
      case (n1, n2) =>
        verify(n1)
        verify(n2)
    }
  }
}
