package s99

import scala.annotation.tailrec

/**
 * Created by korneliusz on 31.12.14.
 */
object P01 {
  @tailrec
  def last[A](list: List[A]): Option[A] = list match {
    case head :: Nil => Some(head)
    case Nil => None
    case _ => last(list.tail)
  }
}
