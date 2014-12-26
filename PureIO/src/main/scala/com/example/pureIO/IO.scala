package com.pureIO

// This draws from Haskell's idea of IO.
// This implementation overflows the stack, this can be solved with trampolining.


object IO {
  def putStrLn(s: String): IO[Unit] =
    io(println(s))

  def getLine: IO[String] =
    io(scala.io.StdIn.readLine())

  def io[A](a: => A): IO[A] =
    new IO((rw) => (a, rw))
}

class IO[A](val apply: RealWorld => (A, RealWorld)) {
  def >>[B](b: => IO[B]): IO[B] = >>=(_ => b)

  // Allows using a for comprehension == equivalent of a `do` block in Haskell.
  def flatMap[B](f: A => IO[B]): IO[B] = >>=(f)

  def >>=[B](f: A => IO[B]): IO[B] = new IO[B]({ world =>
    val (a, world2) = apply(world)
    f(a).apply(world2)
  })

  def map[B](f: A => B): IO[B] = >>=(a => IO.io(f(a)))
}

