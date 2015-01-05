package com.s99

import scala.annotation.tailrec

object P07 {

  def flatten(list: List[Any]): List[Any] = {
    @tailrec
    def go(result: List[Any], rest: List[Any]): List[Any] = rest match {
      case head :: tail => head match {
        case x: List[Any] => go(result ::: flatten(x), tail)
        case x: Any => go(result :+ x, tail)
      }

      case Nil => result
    }

    go(Nil, list)
  }

}
