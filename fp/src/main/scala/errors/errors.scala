package fp.errors

object Errors {
  def mean(xs: Seq[Double]): Option[Double] = xs match {
    case Nil => None
    case seq => Some(seq.foldLeft(0.0)(_ + _) / seq.length.toDouble)
  }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap((m) => mean(xs map ((x) => math.pow(x - m, 2))))

  /*
   Thanks to this we do not have to infect our code with Option.
   */
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    va <- a
    vb <- b
  } yield (f(va, vb))

  // A way to cut exceptions.
  // Note that the argument must be lazy.
  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch { case e: Exception => None }
  }

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap((hh) => sequence(t) map (hh :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldLeft(Some(Nil): Option[List[B]])((r, a) => r match {
      case None => None
      case Some(list) => f(a) match {
        case None => None
        case Some(b) => Some(list ++ List(b))
      }
    })

  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse2(t)(f))(_ :: _)
  }

  def sequence2[A](as: List[Option[A]]): Option[List[A]] =
    traverse2(as)((a) => a)

}
