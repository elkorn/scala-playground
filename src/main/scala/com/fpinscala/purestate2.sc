

object purestate2 {
  // A computation that carries some state along == state action | state transition | statement.

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
}

