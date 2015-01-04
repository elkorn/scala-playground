package com.s99

import scala.annotation.tailrec

object P05 {

  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def go(result: List[A], rest: List[A]): List[A] = rest match {
      case Nil => result
      case head :: tail => go(head :: result, tail)
    }

    go(Nil:List[A], list)
  }

}
