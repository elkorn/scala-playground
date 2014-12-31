package s99

/**
 * Created by korneliusz on 31.12.14.
 */
object P03 {
  def nth[A](n: Int, list: List[A]): Option[A] = n match {
    case _ if (list == Nil) => None
    case 0 => Some(list.head)
    case _ => nth(n-1, list.tail)
  }
}
