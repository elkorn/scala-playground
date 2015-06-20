package fp.property.domain

import fp.Lazy.Stream
import fp.Lazy.Cons
import fp.Lazy.Empty

private[domain] class FiniteDomain[+A](private[domain] val as: Stream[A], private[domain]totalSize: Int) extends Domain[A /*, List*/ ] {
  def isFinite = true
  def isExhausted = size == 0
  def head = as match {
    case Cons(h, t) => h
    case Empty => throw new NoSuchElementException("Head of empty stream")
  }

  def tail: FiniteDomain[A] = as match {
    case Cons(h, t) => new FiniteDomain(t(), size - 1)
    case Empty => throw new NoSuchElementException("Head of empty stream")
  }

  def size = totalSize
  def take(n: Int) = new FiniteDomain(as.take(n), n min size)
  def flatMap[B](f: A => FiniteDomain[B]): FiniteDomain[B] = new FiniteDomain(as.map(f(_).head()), size)
  // FIXME it should look like this.
  // def flatMap[B](f: A => FiniteDomain[B]): FiniteDomain[B] = new FiniteDomain(as.flatMap(f(_).finite.as), size)
  def map[B](f: A => B) = new FiniteDomain(as map f, size)
  def map2[B, C](db: FiniteDomain[B])(f: (A, B) => C) = FiniteDomain.map2(this, db)(f)
  def foldRight[B](z: B)(f: (A, => B) => B) = as.foldRight(z)(f)
  def toList() = Some(as.take(size).toList)
  def finite = this
  def infinite = throw new ClassCastException("Trying to cast a finite domain to an infinite domain")
}

object FiniteDomain {
  def unapply[A](domain: FiniteDomain[A]) =
    if (domain.size == 1) Some((domain.head, EmptyDomain))
    else Domain.unapply(domain)

  def apply[A](as: List[A]) = new FiniteDomain(Stream(as: _*), as.length)
  def map2[A, B, C](da: FiniteDomain[A], db: FiniteDomain[B])(f: (A, B) => C): FiniteDomain[C] = for {
    a <- da
    b <- db
  } yield f(a, b)

  // private def cartesianList[A](as: List[List[A]]): List[List[A]] = as match {
  //   case Nil => List(Nil)
  //   case hs :: tss => for {
  //     h <- hs
  //     ts <- cartesianList(tss)
  //   } yield h :: ts
  // }

  // def cartesian[A](domain: FiniteDomain[FiniteDomain[A]]): FiniteDomain[FiniteDomain[A]] = {
  //   val list = cartesianList(domain.toList.map(_.toList))
  //   FiniteDomain(list.map(FiniteDomain(_)))
  // }

  // def cartesian[A](domain: FiniteDomain[InfiniteDomain[A]]): FiniteDomain[InfiniteDomain[A]] = {
  //   val str = 

  // }
  // InfiniteDomain.cartesian(InfiniteDomain(Stream(domain.toList:_*))).take(domain.size)
}
