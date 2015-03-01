package com.fp.purestate

object State {
  def unit[S, A](value: A): State[S, A] = State((value, _))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State(
    s => {
      fs.foldLeft((Nil: List[A], s))((res, r) => {
        val (n, s2) = r.run(res._2)
        (res._1 :+ n, s2)
      })
    })

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](value: S): State[S, Unit] = State(_ => ((), value))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def flatMap[B](g: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    g(a).run(s2)
  })

  def map2basic[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a =>
      sb.map { b =>
        f(a, b)
      }
    }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- this
      b <- sb
    } yield f(a, b)
}