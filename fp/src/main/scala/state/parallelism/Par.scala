package fp.state.parallelism

case class Par[A](value: A)

object Par {
  def unit[A](a: => A): Par[A] = Par(a)
  def get[A](par: Par[A]): A = par.value

  def sum(ints: IndexedSeq[Int]): Int = {
    if (ints.length <= 1) ints.headOption.getOrElse(0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL = Par.unit(sum(l))
      val sumR = Par.unit(sum(r))
      Par.get(sumL) + Par.get(sumR)
    }
  }
}
