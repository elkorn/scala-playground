package fp.functionalDataStructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def maximum(t: Tree[Int]): Int = {
    def go(currentMax: Int, todo: Tree[Int]): Int = todo match {
      case Leaf(v) => v max currentMax
      case Branch(l, r) => go(currentMax, l) max go(currentMax, r)
    }

    go(Int.MinValue, t)
  }

  def depth[A](t: Tree[A]): Int = {
    def go(currentDepth: Int, todo: Tree[A]): Int = {
      println(currentDepth, todo)
      todo match {
        case Leaf(_) => currentDepth
        case Branch(l, r) => go(currentDepth + 1, l) max go(currentDepth + 1, r)
      }
    }
    go(0, t)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(aggr: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => aggr(fold(l)(f)(aggr), fold(r)(f)(aggr))
  }

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def depth2[A](t: Tree[A]): Int =
    fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))

  def maximum2(t: Tree[Int]): Int =
    fold(t)(a => a)((v1, v2) => v1 max v2)

  def size2[A](t: Tree[A]): Int =
    fold(t)(a => 1)((s1, s2) => s1 + s2 + 1)
}
