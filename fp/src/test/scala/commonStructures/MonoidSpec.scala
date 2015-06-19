package fp.commonStructures

import org.scalatest._
import fp.property.{ Gen, Prop }

class MonoidSpec extends FlatSpec with Matchers {
  "optionMonoid" should "fulfill monoid laws" in {
    val optionLaws =
      Monoid.laws(
        Monoid.optionMonoid[Int],
        Gen.choose(0, 100).map(Option(_))
      )
    Prop.check(optionLaws) should equal(Gen.Result.Proven)
  }

  "endoMonoid" should "fulfill monoid laws" in {
    val endoGen: Gen[Int => Int] = Gen.choose(1, 100).map(n => (m: Int) => n * m)
    val endoLaws =
      Monoid.laws(
        Monoid.endoMonoid[Int],
        endoGen,
        Gen.choose(0, 100)
      )
    Prop.check(endoLaws) should equal(Gen.Result.Proven)
  }
}
