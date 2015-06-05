package fp.property.domain

import fp.Lazy.Stream

object Domain {
  // Stream is sealed - have to deal with that somehow.
  // A Domain should expose the same methods as a stream and delegate most of its methods to the underlying stream.
  // Calling the head getter of the stream should return a new Domain of a reduced size.
  // Also, calling methods from the `take*` and `drop*` family should modify the counter accordingly.

  case object EmptyDomain extends FiniteDomain[Nothing](Nil)

  private[domain] class FiniteDomain[+A](private val as: List[A]) extends Domain[A /*, List*/ ] {
    def isFinite = true
    def isExhausted = as.isEmpty
    def head = () => as.head
    def tail: FiniteDomain[A] = new FiniteDomain(as.tail)
    def size = as.length
    def take(n: Int) = new FiniteDomain(as.take(n))
    def flatMap[B](f: A => List[B]) = new FiniteDomain(as flatMap f)
    def map[B](f: A => B) = new FiniteDomain(as map f)
  }

  object FiniteDomain {
    def unapply[A](domain: FiniteDomain[A]) = if (domain.tail.isExhausted) Some((domain.head, EmptyDomain))
    else Domain.unapply(domain)

    def apply[A](as: A*) = new FiniteDomain(as.toList)
    def apply[A](as: List[A]) = new FiniteDomain(as)
    def constant[B](b: List[B]) = new FiniteDomain(b)
  }

  class InfiniteDomain[+A](private val s: Stream[A]) extends Domain[A /*, Stream*/ ] {
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

    def take(n: Int) = new FiniteDomain(s.take(n).toList)
    def flatMap[B](f: A => Stream[B]) = new InfiniteDomain(s flatMap f)
    def map[B](f: A => B) = new InfiniteDomain(s map f)

    s match {
      case fp.Lazy.Empty => throw new RuntimeException("Cannot use a finite stream in an infinite domain.")
      case _ =>
    }
  }

  object InfiniteDomain {
    def unapply[A](domain: InfiniteDomain[A]) = Domain.unapply(domain)
    def apply[A](s: Stream[A]) = new InfiniteDomain(s)
    // def constant[B](b: B) = new InfiniteDomain(Stream.constant(b))
  }

  def unapply[A /*, M[_]*/ ](domain: Domain[A /*, M*/ ]) = Some((domain.head, domain.tail))
}

trait Domain[+A /*, M[_]*/ ] {
  def isFinite: Boolean
  def isExhausted: Boolean
  def head: () => A
  def tail: Domain[A /*, M*/ ]
  def take(n: Int): Domain.FiniteDomain[A]
  // def flatMap[B](f: A => M[B]): Domain[B, M]
  def map[B](f: A => B): Domain[B /*, M*/ ]
}
