package fp.functionalDataStructures

import scala.annotation.tailrec

/*
    +A - covariant -> if X is a subtype of Y, then List[X] is a
    subtype of List[Y].
   -A - contravariant -> if X is a subtype of Y then List[X] is a supertype of List[Y].
  */
sealed trait List[+A] {
  def tail(): List[A] = this.drop(1)
  def drop(n: Int): List[A] = {
    if (n <= 0) this
    else this match {
      case Nil => sys.error("drop on empty list")
      case Cons(_, tail) => tail.drop(n - 1)
    }
  }
}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, rest: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, List(as.tail: _*))

  def setHead[A](list: List[A], newHead: A): List[A] = list match {
    case Nil => Nil
    case Cons(_, tail) => Cons(newHead, tail)
  }

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) if (f(head)) => dropWhile(tail)(f)
    case list => list
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(head, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  /*
     Right-associative folding.
     */
  def foldRight[A, B](as: List[A], zero: B)(f: (A, B) => B): B = {
    as match {
      case Nil => zero
      case Cons(head, tail) => f(head, foldRight(tail, zero)(f))
    }
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((a, b) => b + 1)

  /*
     Left-associative folding.
     */
  @tailrec
  def foldLeft[A, B](as: List[A], zero: B)(f: (B, A) => B): B = {
    as match {
      case Nil => zero
      case Cons(head, tail) => foldLeft(tail, f(zero, head))(f)
    }
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def foldRight2[A, B](l: List[A], zero: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), zero)((b: B, a: A) => f(a, b))

  def append2[A](l: List[A], r: List[A]): List[A] =
    foldRight2(l, r)(Cons(_, _))

  // Not stack-safe.
  def foldRight3[A, B](l: List[A], zero: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((fn, a) => b => fn(f(a, b)))(zero)

  def concat[A](ls: List[List[A]]): List[A] =
    foldLeft(ls, Nil: List[A])((ls, l) => append2(ls, l))

  def map[A, B](l: List[A])(f: (A) => B): List[B] =
    foldRight2(l, Nil: List[B])((a, b) => Cons(f(a), b))

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight2(l, Nil: List[B])((a, b) => append(f(a), b))

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
    (a, b) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case Cons(hsup, tsup) => sub match {
      case Nil => true
      case Cons(hsub, Nil) => hsup == hsub || hasSubsequence(tsup, sub)
      case Cons(hsub, tsub) =>
        if (hsup == hsub) hasSubsequence(tsup, tsub)
        else hasSubsequence(tsup, sub)
    }
  }
}
