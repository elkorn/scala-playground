package com.s99

import scala.annotation.tailrec

object P06 {

  def isPalindrome[A](list: List[A]): Boolean = {
    @tailrec
    def go(left: Int, right: Int, list: List[A]): Boolean = {
      if(left == right) true
      else if (list.isDefinedAt(left) && list.isDefinedAt(right) && list(left) == list(right)) go(left+1, right-1, list)
      else false
    }

    go(0, list.length-1, list)
  }

}
