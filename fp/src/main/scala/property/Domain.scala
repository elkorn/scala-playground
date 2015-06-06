package fp.property.domain

import fp.Lazy.Stream

object Domain {
  // Stream is sealed - have to deal with that somehow.
  // A Domain should expose the same methods as a stream and delegate most of its methods to the underlying stream.
  // Calling the head getter of the stream should return a new Domain of a reduced size.
  // Also, calling methods from the `take*` and `drop*` family should modify the counter accordingly.

  def unapply[A /*, M[_]*/ ](domain: Domain[A /*, M*/ ]) = Some((domain.head, domain.tail))

  def cartesian[A](domain: Domain[Stream[A]]): Domain[Stream[A]] = {
    val cart = cartesianStream(domain.as)
    domain match {
      case x @ FiniteDomain(_, _) => new FiniteDomain(cart, x.size)
      case x @ InfiniteDomain(_, _) => new InfiniteDomain(cart)
      case x => throw new RuntimeException(s"Unsupported type combination for cartesian: ${x.getClass}")
    }
  }

  private[domain] def cartesianStream[A](s: Stream[Stream[A]]): Stream[Stream[A]] =
    s.foldRight(Stream(Stream[A]()))((h, t) => Stream.map2(h, t)(Stream.cons(_, _)))
}

trait Domain[+A /*, M[_]*/ ] {
  def isFinite: Boolean
  def isExhausted: Boolean
  // TODO Option[A]
  def head: () => A
  def tail: Domain[A /*, M*/ ]
  def take(n: Int): FiniteDomain[A]
  // def flatMap[B](f: A => M[B]): Domain[B, M]
  def map[B](f: A => B): Domain[B /*, M*/ ]
  def foldRight[B](z: B)(f: (A, => B) => B): B
  // def toListOption: Option[List[A]]
  def finite: FiniteDomain[A]
  def infinite: InfiniteDomain[A]
  private[domain] val as: Stream[A]
}
