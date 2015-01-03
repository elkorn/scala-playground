package com.s99

import scala.annotation.tailrec

object P04 {

  def length[A](list: List[A]): Int = {
    @tailrec
    def go[A](list: List[A], length: Int): Int = list match {
      case Nil => length
      case head :: Nil => length + 1
      case head :: tail => go(tail, length+1)
    }

    go(list, 0)
  }

}
