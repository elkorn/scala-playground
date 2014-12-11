import scala.collection.immutable.Stream.Empty

object laziness2 {
  // 5.10
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((head, state)) => Stream.cons(head, unfold(state)(f))
    case None => Stream.empty
  }

  // 5.11
  def fibs2(): Stream[Int] = unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1)))}

  fibs2().take(6).toList

  def from2(n: Int): Stream[Int] = unfold(n) { case nn => Some(nn, nn + 1)}

  from2(5).take(6).toList

  def consts2(n: Int): Stream[Int] = unfold(n) { case nn => Some(nn, nn)}

  consts2(5).take(6).toList

  def ones2(): Stream[Int] = consts2(1)

  // 5.12
  def map2[A, B](s: Stream[A])(f: A => B): Stream[B] = unfold(s) {
    case Stream.cons(head, tail) => Some(f(head), tail)
    case _ => None
  }


  map2(from2(1).take(5))(_ + 3).toList

  def take2[A](s: Stream[A], n: Int): Stream[A] = unfold(s) {
    case Stream.cons(head, tail) if n > 0 => Some(head, take2(tail, n - 1))
    case _ => None
  }

  take2(map2(from2(1).take(5))(_ + 3), 2).toList

  def takeWhile2[A](s: Stream[A])(p: A => Boolean): Stream[A] = unfold(s) {
    case Stream.cons(head, tail) if p(head) => Some(head, takeWhile2(tail)(p))
    case _ => None
  }

  def zipWith[A, B, C](as: Stream[A], bs: Stream[B])(f: (A, B) => C): Stream[C] = unfold((as, bs)) {
    case (Stream.cons(ha, ta), Stream.cons(hb, tb)) => Some(f(ha, hb), (ta, tb))
    case _ => None
  }

  zipWith(Stream(1, 2, 3, 4, 5), Stream(3, 4, 5, 6))(_ + _).toList

  def zipAllWith[A, B, C](as: Stream[A], bs: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = unfold((as, bs)) {
    case (Stream.cons(ha, ta), Stream.cons(hb, tb)) => Some(f(Some(ha), Some(hb)), (ta, tb))
    case (Empty, Stream.cons(hb, tb)) => Some(f(None, Some(hb)), (Empty, tb))
    case (Stream.cons(ha, ta), Empty) => Some(f(Some(ha), None), (ta, Empty))
    case _ => None
  }

  zipAllWith(Stream(1, 2, 3, 4, 5), Stream(3, 4, 6))((_, _)).toList

  def startsWith[A](s1: Stream[A], s2: Stream[A]): Boolean =
    takeWhile2(zipAllWith(s1, s2)((_, _)))(!_._2.nonEmpty).forall {
      case (Some(a), Some(b)) => a == b
      case _ => false
    }

  def startsWith2[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.map(Some(_))
      .zipAll(s2.map(Some(_)), None, None)
      .takeWhile({
      case (Some(a), Some(b)) => true
      case _ => false
    })
      .forall({
      case (Some(a), Some(b)) => a == b
      case _ => false
    })

  startsWith(Stream(1, 2, 3, 4, 5), Stream(1, 2, 3))
  startsWith(Stream(1, 2, 3, 4, 5), Stream(2, 3))
  startsWith2(Stream(1, 2, 3, 4, 5), Stream(1, 2, 3))
  startsWith2(Stream(1, 2, 3, 4, 5), Stream(2, 3))

  def hasSubsequence[A](s: Stream[A], sub: Stream[A]): Boolean = s match {
    case Stream.cons(_, _) if startsWith(s, sub) => true
    case Stream.cons(_, tail) if !startsWith(s, sub) => hasSubsequence(tail, sub)
    case _ => false
  }

  hasSubsequence(Stream(1,2,3,4,5), Stream(4,5))
  hasSubsequence(Stream(1,2,3,4,5), Stream(5,4))
}