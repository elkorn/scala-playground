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

  "concatenate" should "work for folding" in {
    Prop.check(Gen.forAll(Gen.listOfN(10, Gen.choose(10, 20))) { list =>
      Monoid.concatenate(list, Monoid.intAddition) == list.foldLeft(0)(_ + _)
    }) should equal(Gen.Result.Proven)
  }

  "foldMap" should "apply the mapping fn and fold" in {
    Prop.check(Gen.forAll(Gen.listOfN(10, Gen.choose(10, 20))) { list =>
      Monoid.foldMap(list, Monoid.string)(_.toString) == list.map(_.toString).foldLeft("")(_ + _)
    }) should equal(Gen.Result.Proven)
  }

  "ordered" should "determine whether an indexed seq is ordered" in {
    Monoid.ordered(List(1, 2, 3, 4, 5).toIndexedSeq)(_ <= _) should equal(true)
    Monoid.ordered(List(1, 2, 3, 4, 5).reverse.toIndexedSeq)(_ <= _) should equal(false)
  }
}
