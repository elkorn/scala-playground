

object purestate2 {
  val simpleRng: RNG = RNG.simple(2)

  // 6.10
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

  // How could I use Rand here idiomatically?
  State.unit[RNG, Int](12).map2(State.unit(3))(_ + _).run(simpleRng)
  State.sequence[RNG, Int](List(State.unit(12), State.unit(3))).run(simpleRng)

  // The difference between get, set and modify:
  val s1 = State.get[Int]
  val s2 = State.set(12)
  val s3 = State.modify[Int](s => s + 2)
  val testVal = 8

  s1.run(testVal)
  s2.run(testVal)
  s3.run(testVal)


  trait RNG {
    /**
     * Returning the state that would be mutated in place in OOP is a general pattern in pure FP.
     * @return a random number and the new internal state of the generator.
     */
    def nextInt: (Int, RNG)
  }

  object RNG {
    def simple(seed: Long): RNG = new RNG {
      def nextInt = {
        // http://en.wikipedia.org/wiki/Linear_congruential_generator
        val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
      }
    }
  }

  type Rand[A] = State[RNG, A]

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object Candy {
    /**
     * Describes how a vending machine reacts when its knob is turned or a coin inserted.
     * @param inputs actions performed on the machine
     * @return description of computations taking place in response to given actions.
     */
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
      // This generates a list of state modification operations, coupled with a nothing-value (no intermediate results are required).
      val states = inputs.map(asStateModification)
      // This flattens the list of state modifications to a single State instance.
      val seq = State.sequence(states)
      // This returns a function that performs the operations represented by seq and extracts a final State.
      makeResultGetter(seq) { machine => (machine.coins, machine.candies)}
    }

    private def makeResultGetter[Result](seq: State[Machine, Any])(f: Machine => Result): State[Machine, Result] = {
      makeStateGetter(seq).map(f)
    }

    private def makeStateGetter(seq: State[Machine, Any]): State[Machine, Machine] = {
      seq.flatMap(_ => State.get)
    }

    private def asStateModification: (Input) => State[Machine, Unit] = {
      input => State.modify(changeState(input))
    }

    private def changeState(input: Input): (Machine) => Machine = {
      (state: Machine) => (input, state) match {
        case (_, Machine(_, 0, _)) => state
        case (Coin, Machine(true, candies, coins)) => acceptCoin(candies, coins)
        case (Turn, Machine(false, candies, coins)) => dispenseCandy(candies, coins)
        case (Turn, Machine(true, _, _)) => state
        case (Coin, Machine(false, _, _)) => state
      }
    }

    private def dispenseCandy(candies: Int, coins: Int): Machine = {
      Machine(locked = true, candies - 1, coins)
    }

    private def acceptCoin(candies: Int, coins: Int): Machine = {
      Machine(locked = false, candies, coins + 1)
    }
  }


  Candy.simulateMachine(List(Coin, Turn, Coin, Coin, Turn, Turn, Coin, Turn)).run(Machine(locked = true, 12, 33))
}

