package fp.commonStructures

import fp.property.Gen
import fp.property.Prop

/*
 A monoid consists of:
 - some type A
 - an associative binary operation that takes two values of type A and combines them into one
 - a value of type A that is an identity for that operation

 It is crucial to understand that the monoid is just the algebra, i.e. the types and the operations with their laws.
 Any more concrete representations are imprecise.
 */

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val string = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil: List[A]
  }

  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
    val zero = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a2 compose a1
    def zero = (a: A) => a
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as.map(f), m)

  private object Laws {
    def supportsAssociativity[A](m: Monoid[A])(a1: A, a2: A, a3: A) =
      m.op(m.op(a1, a2), a3) == m.op(a1, m.op(a2, a3))

    def supportsIdentity[A](m: Monoid[A])(a: A) =
      m.op(a, m.zero) == a && m.op(m.zero, a) == a

    def supportsAssociativity[A](m: Monoid[A => A])(a1: A => A, a2: A => A, a3: A => A)(v: A) =
      m.op(m.op(a1, a2), a3)(v) == m.op(a1, m.op(a2, a3))(v)

    def supportsIdentity[A](m: Monoid[A => A])(a: A => A)(v: A) =
      m.op(a, m.zero)(v) == a(v) && m.op(m.zero, a)(v) == a(v)

  }

  def laws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    import Laws._
    Gen.forAll(gen ** gen ** gen) {
      case ((a1, a2), a3) =>
        supportsAssociativity(m)(a1, a2, a3) && supportsIdentity(m)(a1)
    }
  }

  def laws[A](m: Monoid[A => A], endoGen: Gen[A => A], valueGen: Gen[A]): Prop = {
    import Laws._
    Gen.forAll(endoGen ** endoGen ** endoGen ** valueGen) {
      case (((a1, a2), a3), v) =>
        supportsAssociativity(m)(a1, a2, a3)(v) && supportsIdentity(m)(a1)(v)
    }
  }
}
