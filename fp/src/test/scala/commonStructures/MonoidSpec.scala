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

  "product" should "zip two monoids into a tuple" in {
    val plusMult = Monoid.product(Monoid.intAddition, Monoid.intMultiplication)
    plusMult.op((3, 7), (5, 8)) should equal((8, 56))
    plusMult.zero should equal((0, 1))
  }

  "mapMergeValues" should "use a monoid to merge two maps" in {
    Monoid.mapMergeValues(Monoid.intAddition).op(Map("a" -> 112, "b" -> 13), Map("a" -> 57, "b" -> 6)) should equal(Map("a" -> 169, "b" -> 19))
  }

  "function" should "handle functions with another monoid" in {
    Monoid.function(Monoid.intMultiplication).op((n: Int) => n + 8, (n: Int) => n + 9)(13) should equal((13 + 8) * (13 + 9))
    Monoid.function(Monoid.intMultiplication).zero(13) should equal(Monoid.intMultiplication.zero)
  }

  "bag" should "count the occurences of elements in a sequence" in {
    Monoid.bag(Vector('1', '2', '3', '1')) should equal(Map('1' -> 2, '2' -> 1, '3' -> 1))
    Monoid.bag(Vector("a", "rose", "is", "a", "rose")) should equal(Map("a" -> 2, "rose" -> 2, "is" -> 1))
  }

  "bag_2" should "behave the same as bag" in {
    Monoid.bag_2(Vector('1', '2', '3', '1')) should equal(Monoid.bag(Vector('1', '2', '3', '1')))
    Monoid.bag_2(Vector("a", "rose", "is", "a", "rose")) should equal(Monoid.bag(Vector("a", "rose", "is", "a", "rose")))
  }

  "mean" should "compute the mean of an iterable" in {
    val input = List(1, 2, 3, 4, 5)
    Monoid.mean(input) should equal(input.sum / input.length.toDouble)
  }
}
