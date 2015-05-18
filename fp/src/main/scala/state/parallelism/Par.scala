package fp.state.parallelism

case class Par[A](value: A)

object Par {
  def unit[A](a: => A): Par[A] = Par(a)
  def get[A](par: Par[A]): A = par.value

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.length <= 1) unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(sum(l), sum(r))(_ + _)
    }
  }

  def map2[A, B, C](pa: => Par[A], pb: => Par[B])(f: (A, B) => C): Par[C] =
    unit(f(get(pa), get(pb)))
}
