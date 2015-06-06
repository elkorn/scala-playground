package fp.property.domain

import fp.Lazy.Stream

object Domain {
  // Stream is sealed - have to deal with that somehow.
  // A Domain should expose the same methods as a stream and delegate most of its methods to the underlying stream.
  // Calling the head getter of the stream should return a new Domain of a reduced size.
  // Also, calling methods from the `take*` and `drop*` family should modify the counter accordingly.

  def unapply[A /*, M[_]*/ ](domain: Domain[A /*, M*/ ]) = Some((domain.head, domain.tail))
}

trait Domain[+A /*, M[_]*/ ] {
  def isFinite: Boolean
  def isExhausted: Boolean
  def head: () => A
  def tail: Domain[A /*, M*/ ]
  def take(n: Int): FiniteDomain[A]
  // def flatMap[B](f: A => M[B]): Domain[B, M]
  def map[B](f: A => B): Domain[B /*, M*/ ]
}
