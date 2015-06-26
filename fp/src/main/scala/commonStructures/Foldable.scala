package fp.commonStructures

import scala.annotation.tailrec
import fp.Lazy.Stream

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
}

object Foldable {
  val list = new Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      foldLeft(as.reverse)(z)((b, a) => f(a, b))

    @tailrec
    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case h :: t => foldLeft(t)(f(z, h))(f)
    }

    def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(f(a), b))
  }

  val indexedSeq = new Foldable[IndexedSeq] {
    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))
  }

  val stream = new Foldable[Stream] {
    def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f(_, _))

    def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f(_, _))

    def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
      as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))
  }
}
