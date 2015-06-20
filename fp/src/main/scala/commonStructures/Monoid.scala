package fp.commonStructures

import fp.property.Gen
import fp.property.Prop
import fp.state.parallelism.Par
import fp.state.parallelism.parallelism.Par

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

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(pa1: Par[A], pa2: Par[A]): Par[A] =
      Par.flatMap(pa1)(a1 => Par.map(pa2)(a2 => m.op(a1, a2)))
    def zero = Par.unit(m.zero)
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(b, a))(z)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.length == 1) f(v.head)
    else {
      val (left, right) = v.splitAt(v.length / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }

  def parFoldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.flatMap(Par.parMap(v)(f))(bs => foldMapV(bs, par(m))(Par.lazyUnit(_)))

  def ordered(ns: IndexedSeq[Int])(order: (Int, Int) => Boolean): Boolean = {
    val monoid = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) = (o1, o2) match {
        case (Some((min1, max1, p)), Some((min2, max2, q))) =>
          // It doesn't really matter whether min or max comes first as long as the ordering condition is maintained.
          // It could also be:
          // Some(min1 max min2, max1 min max2, p && q && min1 <= max2)
          Some((min1 min min2, max1 max max2, p && q && order(min1, max2)))
        case (x, None) => x
        case (None, x) => x
      }

      val zero = None
    }

    // (n => Some(n,n,true)) -> each element by itself is ordered
    // .getOrElse(true)      -> empty sequence is ordered
    foldMapV(ns, monoid)(n => Some(n, n, true)).map(t => t._3).getOrElse(true)
  }

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
