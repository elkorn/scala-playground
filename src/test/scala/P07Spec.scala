package org.p99.scala

import com.s99.P07
import org.scalatest._

class P07Spec extends UnitSpec with Matchers {

  it should "flatten a list" in {
    P07.flatten(List(List(1,2,3), List(4,5,6, List(7,8,9)))) should be(List(1,2,3,4,5,6,7,8,9))
    P07.flatten(List(List(List(1), List(2, List(3))))) should be(List(1,2,3))
  }

}

