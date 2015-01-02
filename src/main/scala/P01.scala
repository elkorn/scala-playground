package org.p99.scala

import scala.annotation.tailrec

object P01 {
  @tailrec
  def last[A](list: List[A]): Option[A] = list match {
    case head :: Nil => Some(head)
    case Nil => None
    case _ => last(list.tail)
  }
}
