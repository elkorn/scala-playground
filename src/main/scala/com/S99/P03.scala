package com.S99

import scala.annotation.tailrec

object P03 {
  def nth[A](n: Int, list: List[A]): Option[A] = n match {
    case _ if (list == Nil) => None
    case 0 => Some(list.head)
    case _ => nth(n-1, list.tail)
  }
}