package fp.state

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(
    s => {
      val (a, s2) = run(s)
      (f(a), s2)
    }
  )

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s1 => {
      val (a, s2) = run(s1)
      val (b, s3) = sb.run(s2)

      (f(a, b), s3)
    })

  def map2_2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap((a) => sb.map((b) => f(a, b)))

  // Using a for comprehension allows getting rid of explicit state passing completely.
  def map2_3[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = for {
    a <- this
    b <- sb
  } yield (f(a, b))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s1 => {
      val (a, s2) = run(s1)
      f(a).run(s2)
    })
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s1 => {
      println(s"s1: $s1")
      val (a, s2) = sa.run(s1)
      println(s"a: $a, s2: $s2")
      val (b, s3) = sb.run(s2)
      println(s"b: $b, s3: $s3")
      val result = (f(a, b), s3)
      println("map2 is done.")
      result
    })

  // Walks the list once, not tail-recursive
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](Nil: List[A]))((sa, acc) => sa.map2(acc)(_ :: _))

  // Walks the list twice, not tail-recursive
  def sequence2[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s0: S, todo: List[State[S, A]], result: List[A]): (List[A], S) = todo match {
      case h :: t => {
        val (a, s1) = h.run(s0)
        go(s1, t, a :: result)

      }
      case Nil => (result.reverse, s0)
    }

    State(s => go(s, sas, Nil: List[A]))
  }

  // Walks the list twice, tail-recursive
  def sequence3[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.reverse.foldLeft(unit[S, List[A]](Nil))((acc, sa) => sa.map2(acc)(_ :: _))

  // Promote the state to a result.
  def get[S]: State[S, S] = State(s => (s, s))

  // Override the state for any given transition.
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // Use a function to change the state 'in-place', sort of.
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}
