package com.example

object ListHOFs {
  def foldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): B = {
    as match {
      case Nil => acc
      case x :: xs => f(x, foldRight(xs, acc)(f))
    }
  }

  def sum(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  def product(ns: List[Int]) = foldRight(ns, 1)(_ * _)
}
