package com.pureIO.FreeMonad

import com.pureIO.FreeMonad

object AnyEffect {

  type AnyIO[A] = FreeMonad.IO[Runnable, A]

  def delay[A](a: => A) =
    new Runnable[A] {
      def run = a
    }

  trait Runnable[A] {
    def run: A
  }

}
