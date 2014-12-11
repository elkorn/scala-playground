

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
}