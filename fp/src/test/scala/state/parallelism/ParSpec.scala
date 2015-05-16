package fp.state.parallelism

import org.scalatest._

import Par._

class ParSpec extends FlatSpec with Matchers {
  "sum" should "sum a sequence of integers" in {
    sum(IndexedSeq(1, 2, 3, 4, 5)) should equal(1 + 2 + 3 + 4 + 5)

  }

}
