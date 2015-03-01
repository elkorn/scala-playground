package com.fp.monoids

import com.fp.testing.{Gen, Prop, SGen}

/**
 * A monoid is defined only by its algebra.
 * Other than satisfying the same laws, instances of the monoid interface do not have to have anything in common.
 *
 * A monoid consists of the following:
 * - some type A
 * - an associative binary operation `op` that takes two values of type A and combines them into one:
 * `op(op(x,y),z) == op(x,op(y,z))` for any choice of `x,y,z`.
 * - A value `zero: A`, that is an identity for that operation: `op(zero, x) == x` and `op(x, zero) == x` for any `x`.
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
  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }
  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }
  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }
  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def list[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ::: a2

    override def zero: List[A] = Nil
  }

  def option[A]: Monoid[Option[A]] = new Monoid[Option[A]] {

    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endofunction[B])(f.curried)(z)

  def endofunction[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2

    override def zero: (A) => A = (a) => a
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((result, a2) => m.op(result, f(a2)))

  def foldLeft[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, dual(endofunction[B]))(f.curried)(z)

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)

    override def zero: A = m.zero
  }

  def foldMapV[A, B](v: IndexedSeq[A])(m: Monoid[B])(f: A => B): B = {
    if (v.length == 0) m.zero
    else if (v.length == 1) f(v(0))
    else {
      val split = v.splitAt(v.length / 2)
      m.op(foldMapV(split._1)(m)(f), foldMapV(split._2)(m)(f))
    }
  }


//
//  def par[A](m: Monoid[A]): Monoid[Par[A]]
//  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B]



  def monoidLaws[A](m: Monoid[A])(gen: SGen[A]): Prop = {
    def isAssociative(x: A, y: A, z: A): Boolean =
      m.op(x, m.op(y, z)) == m.op(m.op(x, y), z)
    def zeroIsIdentity(x: A): Boolean =
      m.op(m.zero, x) == x && m.op(x, m.zero) == x

    Gen.forAll(gen.zipWith(gen).zipWith(gen)) {
      case ((x, y), z) => isAssociative(x, y, z) && zeroIsIdentity(x)
    }
  }

}

