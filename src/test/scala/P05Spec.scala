package org.p99.scala

import com.s99.P05
import org.scalatest._

class P05Spec extends UnitSpec with Matchers {
  val list = List(1,2,3,4)

  it should "reverse a list" in {
    P05.reverse(list) should be (List(4,3,2,1))
  }

  it should "be symmetric" in {
    P05.reverse(P05.reverse(list)) should be(list)
   }
}

