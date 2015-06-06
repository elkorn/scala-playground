package fp.property.domain

import fp.Lazy.Stream

private[domain] class InfiniteDomain[+A](private[domain] val as: Stream[A]) extends Domain[A /*, Stream*/ ] {
  def isFinite = false
  def isExhausted = false
  def head = as match {
    case fp.Lazy.Cons(h, t) => h
    case _ => throw new RuntimeException("Code execution should not reach this point.")
  }

  def tail: InfiniteDomain[A] = as match {
    case fp.Lazy.Cons(h, t) => new InfiniteDomain(t())
    case _ => throw new RuntimeException("Code execution should not reach this point.")
  }

  def take(n: Int) = FiniteDomain(as.take(n).toList)
  def flatMap[B](f: A => InfiniteDomain[B]) = new InfiniteDomain(as.map(f(_).head()))
  def map[B](f: A => B) = new InfiniteDomain(as map f)
  def map2[B, C](db: InfiniteDomain[B])(f: (A, B) => C) = InfiniteDomain.map2(this, db)(f)
  def foldRight[B](z: B)(f: (A, => B) => B) = as.foldRight(z)(f)
  def toList = None
  def finite = throw new ClassCastException("Trying to cast an infinite domain to a finite domain")
  def infinite = this

  as match {
    case fp.Lazy.Empty => throw new RuntimeException("Cannot use a finite stream in an infinite domain.")
    case _ =>
  }
}

object InfiniteDomain {
  def unapply[A](domain: InfiniteDomain[A]) = Domain.unapply(domain)
  def apply[A](s: Stream[A]) = new InfiniteDomain(s)
  def map2[A, B, C](da: InfiniteDomain[A], db: InfiniteDomain[B])(f: (A, B) => C) = for {
    a <- da
    b <- db
  } yield f(a, b)

  // def cartesian[A](domain: InfiniteDomain[InfiniteDomain[A]]): InfiniteDomain[InfiniteDomain[A]] =
  //   new InfiniteDomain(cartesianStream(domain.as.map(_.as)).map(new InfiniteDomain(_)))

  // def cartesian[A](domain: InfiniteDomain[FiniteDomain[A]]): InfiniteDomain[FiniteDomain[A]] =
  //   new InfiniteDomain(
  //     cartesianStream(domain.as.map(x => Stream(x.as: _*)))
  //       .map(x => new FiniteDomain(x.toList))
  //   )
}
