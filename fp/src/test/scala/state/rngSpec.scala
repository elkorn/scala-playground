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

  "unit" should "return the given value without using a generator" in {
    all((1 to 200).map((_) => RNG.unit(10)(rng)._1)) should equal(10)
  }

  "map" should "apply a mapping fn to the given state transition" in {
    (RNG.map(RNG.unit(12))(_ * 10))(rng)._1 should equal(120)
  }

  "double2" should "return a random double in [0; 1)" in {
    var newRng = rng
    all((1 to 200).map((_) => {
      val (i, r) = RNG.double2(newRng)
      newRng = r
      i
    })) should (be >= 0.0 and be < 1.0)
  }

  "map2" should "return an action that combines the results of two different actions" in {
    RNG.map2(RNG.unit(12), RNG.unit(13))(_ + _)(rng)._1 should equal(25)
  }

  "sequence" should "combine a list of transitions into a single transition" in {
    RNG.sequence(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))(rng)._1 should equal(List(1, 2, 3))
  }

  "sequence2" should "act the same as sequence" in {
    RNG.sequence2(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))(rng)._1 should equal(RNG.sequence(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))(rng)._1)
  }

  "flatMap" should "apply a mapping fn and flatten the result" in {
    RNG.flatMap(RNG.unit(12))((x) => RNG.unit(x + 84))(rng)._1 should equal(96)
  }

  "map_2" should "act the same as map" in {
    (RNG.map_2(RNG.unit(12))(_ * 10))(rng)._1 should equal((RNG.map(RNG.unit(12))(_ * 10))(rng)._1)
  }

  "map2_2" should "act the same as map2" in {
    RNG.map2_2(RNG.unit(12), RNG.unit(13))(_ + _)(rng)._1 should equal(RNG.map2(RNG.unit(12), RNG.unit(13))(_ + _)(rng)._1)
  }

}
