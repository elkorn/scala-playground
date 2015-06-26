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

  def foldLeft[B](z: => B)(f: (=> B, => A) => B): B = Stream.foldLeft(this)(z)(f)

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

  def map2[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def take2(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (_, 0) => None
      case (Empty, _) => None
      case (Cons(h, t), n) => Some((h(), (t(), n - 1)))
    }

  def takeWhile3(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if (p(h())) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](bs: => Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, bs)) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some(f(ha(), hb()), (ta(), tb()))
      case _ => None
    }

  def zip[B](bs: => Stream[B]): Stream[(A, B)] =
    zipWith(bs)((_, _))

  def zipAll[B](bs: => Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, bs)) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
      case (Empty, Cons(h, t)) => Some(((None, Some(h())), (Empty, t())))
      case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), Empty)))
      case _ => None
    }

  def startsWith[A](s: => Stream[A]): Boolean =
    zipAll(s).takeWhile((t) => t._1.isDefined && t._2.isDefined).forAll {
      case (Some(a), Some(b)) => a == b
      case _ => false
    }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case x @ Cons(h, t) => Some(x, t())
      case _ => None
    }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    // Going from the right, we carry over the result of `f`
    // and accumulate in it the results of the intermediate operations.
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b = f(a, p1._1)
      println(Stream.cons(b, p1._2).toList)
      (b, Stream.cons(b, p1._2))
    })._2
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

  def ones(): Stream[Int] = constant(1)

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def foldLeft[A, B](as: Stream[A])(z: => B)(f: (=> B, => A) => B): B = as match {
    case Cons(h, t) => foldLeft(t())(f(z, h()))(f)
    case Empty => z
  }

  def fibs(): Stream[Int] = {
    def go(prev1: Int, prev2: Int): Stream[Int] =
      cons(prev2, go(prev2, prev1 + prev2))
    cons(0, go(0, 1))
  }

  def unfold[A, B](zero: B)(f: B => Option[(A, B)]): Stream[A] = f(zero) match {
    case Some((next, newZero)) => cons(next, unfold(newZero)(f))
    case None => empty
  }

  def fibs2(): Stream[Int] = {
    unfold((0, 1)) {
      case (f1: Int, f2: Int) => Some(f1, (f2, f1 + f2))
    }
  }

  def constant2[A](a: A): Stream[A] =
    unfold(a)((_) => Some(a, a))

  def from2(n: Int): Stream[Int] =
    unfold(n)((r) => Some(r, r + 1))

  def ones2(): Stream[Int] = constant2(1)

  def map2[A, B, C](sa: Stream[A], sb: Stream[B])(f: (A, B) => C): Stream[C] = for {
    a <- sa
    b <- sb
  } yield f(a, b)
}
