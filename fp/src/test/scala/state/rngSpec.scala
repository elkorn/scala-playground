package fp.state

import org.scalatest._
import org.scalatest.Matchers._

class RngSpec extends FlatSpec with Matchers {
  // A property-based test is required to prove the correctness of those.
  val rng: RNG = SimpleRNG(123L)
  "nonNegativeInt" should "return a random, non-negative integer" in {
    var newRng = rng
    all((1 to 200).map((_) => {
      val (i, r) = RNG.nonNegativeInt(newRng)
      newRng = r
      i
    })) should be > 0
  }

  "double" should "return a random double in [0; 1)" in {
    var newRng = rng
    all((1 to 200).map((_) => {
      val (i, r) = RNG.double(newRng)
      newRng = r
      i
    })) should (be >= 0.0 and be < 1.0)
  }

  "ints" should "generate a list of random integers" in {
    RNG.ints(6)(rng)._1 should have size 6
  }

}
