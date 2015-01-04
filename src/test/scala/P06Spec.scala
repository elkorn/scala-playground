package org.p99.scala

import com.s99.P06
import org.scalatest._

class P06Spec extends UnitSpec with Matchers {
  it should "detect a palindrome" in {
    P06.isPalindrome(List(1,2,3,2,1)) should be(true)
  }

  it should "detect a non-palindrome" in {
    P06.isPalindrome(List(1,2,3)) should be(false)
    P06.isPalindrome(List()) should be(false)
  }

}

