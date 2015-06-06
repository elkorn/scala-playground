package fp.property.domain

private[domain] class FiniteDomain[+A](private[domain] val as: List[A]) extends Domain[A /*, List*/ ] {
  def isFinite = true
  def isExhausted = as.isEmpty
  def head = () => as.head
  def tail: FiniteDomain[A] = new FiniteDomain(as.tail)
  def size = as.length
  def take(n: Int) = new FiniteDomain(as.take(n))
  def flatMap[B](f: A => FiniteDomain[B]): FiniteDomain[B] = new FiniteDomain(as.map(f(_).head()))
  def map[B](f: A => B) = new FiniteDomain(as map f)
  def map2[B, C](db: FiniteDomain[B])(f: (A, B) => C) = FiniteDomain.map2(this, db)(f)
  def foldRight[B](z: B)(f: (A, B) => B) = as.foldRight(z)(f)
}

object FiniteDomain {
  def unapply[A](domain: FiniteDomain[A]) = if (domain.tail.isExhausted) Some((domain.head, EmptyDomain))
  else Domain.unapply(domain)

  def apply[A](as: A*) = new FiniteDomain(as.toList)
  def apply[A](as: List[A]) = new FiniteDomain(as)
  def constant[B](b: List[B]) = new FiniteDomain(b)
  def map2[A, B, C](da: FiniteDomain[A], db: FiniteDomain[B])(f: (A, B) => C): FiniteDomain[C] = for {
    a <- da
    b <- db
  } yield f(a, b)

  private def cartesianList[A](as: List[List[A]]): List[List[A]] = as match {
    case Nil => List(Nil)
    case hs :: tss => for {
      h <- hs
      ts <- cartesianList(tss)
    } yield h :: ts
  }

  def cartesian[A](domain: FiniteDomain[FiniteDomain[A]]): FiniteDomain[FiniteDomain[A]] =
    new FiniteDomain(cartesianList(domain.as.map(_.as)).map(FiniteDomain(_)))
}
