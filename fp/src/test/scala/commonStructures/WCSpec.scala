package fp.commonStructures

import org.scalatest._
import fp.property._

class WCSpec extends FlatSpec with Matchers {
  "monoid" should "fulfill monoid laws" in {
    val stubGen: Gen[WC] = Gen.stringN(1).map(WC.Stub(_))
    val partGen: Gen[WC] = (Gen.stringN(1) ** Gen.choose(0, 10) ** Gen.stringN(1)).map(x => WC.Part(x._1._1, x._1._2, x._2))

    Prop.check(Monoid.laws(WC.monoid, stubGen)) should equal(Gen.Result.Proven)
    Prop.check(Monoid.laws(WC.monoid, partGen)) should equal(Gen.Result.Proven)
    Prop.check(Monoid.laws(WC.monoid, Gen.weighted((stubGen, .5), (partGen, .5)))) should equal(Gen.Result.Proven)
  }

  "WC" should "count words in a string" in {
    WC("lorem ipsum dolor sit amet") should equal(5)
    WC("") should equal(0)
    WC("d") should equal(1)
    WC("lorem ipsum,dolor sit amet") should equal(5)
  }
}
