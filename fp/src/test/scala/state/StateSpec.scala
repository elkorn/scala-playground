package fp.state

import org.scalatest._
import org.scalatest.Matchers._

class StateSpec extends FlatSpec with Matchers {
  "unit" should "return a value regardless of state" in {
    State.unit(2).run(3) should equal((2, 3))
  }

  "map" should "apply a mapping fn to a state" in {
    State((s: Int) => (s + 3, s)).map(_ * 2).run(10) should equal((26, 10))

  }
}
