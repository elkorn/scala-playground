package s99

import com.s99.P04
import org.p99.scala.UnitSpec
import org.scalatest._

class P04Spec extends UnitSpec with Matchers {

  it should "count the number of elements in a non-empty list" in {
    val list = List(1, 2, 3, 4, 5, 6)
    P04.length(list) should be(6)
  }

  it should "count the number of elements in an empty list" in {
    val list = List()
    P04.length(list) should be(0)
  }

}

