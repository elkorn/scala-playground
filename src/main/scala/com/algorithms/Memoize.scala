package com.algorithms

/**
 * Created by korneliusz on 06.01.15.
 */
case class Memoize[A, B](f: A => B) {
  private var memo: Map[A, B] = Map()

  def apply(a: A): B = {
    if (!memo.isDefinedAt(a)) {
      val result = f(a)
      memo = memo + (a -> result)
    }

    memo(a)
  }
}
