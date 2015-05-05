package fp.Lazy

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    def go(result: List[A], todo: Stream[A]): List[A] = todo match {
      case Empty => result
      case Cons(h, t) => go(h() :: result, t())
    }

    go(Nil: List[A], this).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n <= 0) Stream.empty
    else Stream.cons(h(), t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n <= 0) this
    else t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => b && p(a))

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty: Stream[A])((a, b) => if (p(a)) Stream.cons(a, b)
    else b)

  // This works, because thanks to the lazy nature of streams, the folding function blocks 
  // all folding iterations except the one with the head from happening.
  def headOption2(): Option[A] =
    foldRight(None: Option[A])((head, _) => Some(head))

  def append[B >: A](value: => Stream[B]): Stream[B] =
    foldRight(value)(Stream.cons(_, _))

  def appendValue[B >: A](value: => B): Stream[B] =
    append(Stream.cons(value, Stream.empty))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => f(a) append b)

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else t)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def go(prev1: Int, prev2: Int): Stream[Int] =
      cons(prev2, go(prev2, prev1 + prev2))
    cons(0, go(0, 1))
  }

  def unfold[A, B](zero: B)(f: B => Option[(A, B)]): Stream[A] = f(zero) match {
    case Some((next, newZero)) => cons(next, unfold(newZero)(f))
    case None => empty
  }
}
