package com.fp.adt

/**
 * Created by elkorn on 1/10/15.
 */
object Tree {

  // Things to fix:
  // - Should be tailrec.
  // - Should allow depth calculation.
  def fold[A, B](t: Tree[A], acc: B)(f: (A, B) => B): B = {
    t match {
      case Leaf(value) => f(value, acc)
      case Branch(left, right) => fold(right, fold(left, acc)(f))(f)
    }
  }

  def maximum2(t: Tree[Int]): Int = fold(t, 0)(_ max _)

  def size2(t: Tree[Int]): Int = fold(t, 0)((_, res) => res + 1)

  def maximum(t: Tree[Int]): Int = {
    var max: Int = Int.MinValue
    t match {
      case Branch(l: Tree[Int], r: Tree[Int]) => maximum(l) max maximum(r)
      case Leaf(v: Int) if v > max =>
        max = v
        v
    }
  }

  case class Leaf[A](value: A) extends Tree[A] {
    def size() = 1

    def depth() = 0

    def map[B](f: A => B) = Leaf(f(value))

    override def toString = value.toString
  }

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
    def size() = left.size + right.size

    def depth() = (left.depth max right.depth) + 1

    def map[B](f: A => B) = Branch(left.map(f), right.map(f))

    override def toString(): String = s"($left ^ $right)"
  }

}

sealed trait Tree[+A] {
  def size(): Int

  def depth(): Int

  def map[B](f: (A) => B): Tree[B]
}
