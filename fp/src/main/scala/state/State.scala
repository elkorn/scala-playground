package fp.state

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(
    s => {
      val (a, s2) = run(s)
      (f(a), s2)
    }
  )
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
}

// // Just making the concept explicit.
// type State[State, +Result] = State => (Result, State)
