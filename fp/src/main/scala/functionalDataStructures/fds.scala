package fp.functionalDataStructures

object Fds {
  /*
    +A - covariant -> if X is a subtype of Y, then List[X] is a
    subtype of List[Y].
   -A - contravariant -> if X is a subtype of Y then List[X] is a supertype of List[Y].
  */
  sealed trait List[+A]

  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, List(as.tail: _*))
  }

}
