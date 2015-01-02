package org.p99.scala

import scala.annotation.tailrec

object P02 {
  @tailrec
  def penultimate[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case penultimate :: List(last) => Some(penultimate)
    case _ => penultimate(list.tail)
  }
}
