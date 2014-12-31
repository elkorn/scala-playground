package s99

import scala.annotation.tailrec

/**
 * Created by korneliusz on 31.12.14.
 */
object P02 {
  @tailrec
  def penultimate[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case penultimate :: List(last) => Some(penultimate)
    case _ => penultimate(list.tail)
  }
}
