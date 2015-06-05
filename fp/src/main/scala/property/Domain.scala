package fp.property

import fp.Lazy.Stream

object Domain {
  // Stream is sealed - have to deal with that somehow.
  // A Domain should expose the same methods as a stream and delegate most of its methods to the underlying stream.
  // Calling the head getter of the stream should return a new Domain of a reduced size.
  // Also, calling methods from the `take*` and `drop*` family should modify the counter accordingly.

  case object EmptyDomain extends FiniteDomain[Nothing](Nil)

  class FiniteDomain[A](private val as: List[A]) extends Domain[A] {
    def isFinite = true
    def isExhausted = as.isEmpty
    def head = () => as.head
    def tail: FiniteDomain[A] = new FiniteDomain(as.tail)
    def size = as.length
  }

  object FiniteDomain {
    def unapply[A](domain: FiniteDomain[A]) = if (domain.tail.isExhausted) Some(domain.head, EmptyDomain)
    else Domain.unapply(domain)
  }

  class InfiniteDomain[A](private val s: Stream[A]) extends Domain[A] {
    def isFinite = false
    def isExhausted = false
    def head = s match {
      case fp.Lazy.Cons(h, t) => h
      case _ => throw new RuntimeException("Code execution should not reach this point.")
    }

    def tail: InfiniteDomain[A] = s match {
      case fp.Lazy.Cons(h, t) => new InfiniteDomain(t())
      case _ => throw new RuntimeException("Code execution should not reach this point.")
    }

    s match {
      case fp.Lazy.Empty => throw new RuntimeException("Cannot use a finite stream in an infinite domain.")
      case _ =>
    }
  }

  object InfiniteDomain {
    def unapply[A](domain: InfiniteDomain[A]) = Domain.unapply(domain)
  }

  def unapply[A](domain: Domain[A]) = Some((domain.head, domain.tail))
}

trait Domain[+A] {
  def isFinite: Boolean
  def isExhausted: Boolean
  def head: () => A
  def tail: Domain[A]
}
