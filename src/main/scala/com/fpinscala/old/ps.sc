object purestate2 {
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

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object Candy {
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
      _ <- State.sequence(inputs.map(input => State.modify((state: Machine) => (input, state) match {
        case (_, Machine(_, 0, _)) => state
        case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
        case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
        case (Turn, Machine(true, _, _)) => state
        case (Coin, Machine(false, _, _)) => state
      })))
      state <- State.get
    } yield (state.coins, state.candies)
  }

  State.get.run(2)

  Candy.simulateMachine(List(Coin,Turn)).run(Machine(true,1,0))
}
