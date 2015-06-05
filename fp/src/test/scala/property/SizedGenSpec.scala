package fp.property

import org.scalatest._

class SizedGenSpec extends FlatSpec with Matchers {
  import Gen._
  "sized generation" should "generate increasingly bigger test samples" in {
    fp.property.Prop.check(forAll(Gen.boolean.unsized)(x => x || !x), maxSize = 100, testCases = 200) should equal(Gen.Result.Exhausted)

  }
}
