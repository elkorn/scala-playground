package com.algorithms

/**
 * Created by elkorn on 1/11/15.
 */
sealed trait Continuation[R, +A] {
  def apply(f: A => R): R

  def map[B](k: A => B) =
    Continuation.continuation[R, B](z => this.apply(z compose k))

  def flatMap[B](k: A => Continuation[R, B]) =
    Continuation.continuation[R, B](z => apply(k(_)(z)))
}

object Continuation {
  def continuation[R, A](g: (A => R) => R): Continuation[R, A] = new Continuation[R, A] {
    def apply(f: A => R) = g(f)
  }

  def unit[R] = new {
    def apply[A](a: A) = continuation[R, A](f => f(a))
  }

  def bind[R, A, B](f: (A => Continuation[R, B]) => Continuation[R, A]) =
    continuation[R, A](k => f(a => continuation(x => k(a)))(k))
}
