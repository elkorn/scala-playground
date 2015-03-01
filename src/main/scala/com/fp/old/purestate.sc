import scala.annotation.tailrec

object purestate {

  // Functions of type RNG => (A, RNG) describe state actions which transform RNG states.
  // These actions can be built up and combined.
  type Rand[+A] = RNG => (A, RNG)

  // A computation that carries some state along == state action | state transition | statement.
  type State[S, +A] = S => (A, S)

  val simpleRng: RNG = RNG.simple(2)

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

  object examples {
    // Reusing the rng allows to have repeatable results.
    def randomPair(rng: RNG): (Int, Int) = {
      val (i1, _) = rng.nextInt
      val (i2, _) = rng.nextInt
      (i1, i2)
    }

    randomPair(simpleRng)

    def randomPair2(rng: RNG): ((Int, Int), RNG) = {
      val (i1, rng2) = rng.nextInt
      val (i2, rng3) = rng2.nextInt
      ((i1, i2), rng3)
    }

    randomPair2(simpleRng)

    // An rng returning a constant.
    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    // Transform the output of a state action without modifying the state itself.
    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    def mapViaGeneralMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
      generalMap(s)(f)

    def generalMap[S, A, B](a: S => (A, S))(f: A => B): S => (B, S) =
      s => {
        val (r, s2) = a(s)
        (f(r), s2)
      }

    def nonNegativeLessThan(n: Int): Rand[Int] =
      exercises.flatMap(exercises.nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod)
        else nonNegativeLessThan(n)
      }

    def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
  }

  object exercises {
    // 6.1
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (nextInt, rng2) = rng.nextInt
      if (nextInt == Int.MinValue) nonNegativeInt(rng2)
      else (nextInt.abs, rng2)
    }

    // 6.2
    def double(rng: RNG): (Double, RNG) = {
      val (result, rng2) = nonNegativeInt(rng)
      (result.toDouble / Int.MaxValue, rng2)
    }

    // 6.3
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (r1, rng2) = nonNegativeInt(rng)
      val (r2, rng3) = double(rng2)

      ((r1, r2), rng3)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val (r1, rng2) = double(rng)
      val (r2, rng3) = nonNegativeInt(rng2)

      ((r1, r2), rng3)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (r1, rng2) = double(rng)
      val (r2, rng3) = double(rng2)
      val (r3, rng4) = double(rng3)

      ((r1, r2, r3), rng4)
    }

    // 6.4
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      @tailrec
      def go(result: (Int, RNG), acc: List[Int], remaining: Int): (List[Int], RNG) = {
        if (remaining == 0) (acc :+ result._1, result._2)
        else {
          go(result._2.nextInt, acc :+ result._1, remaining - 1)
        }
      }

      go(rng.nextInt, Nil: List[Int], count)
    }

    // 6.5
    // we need the resulting function of type Rand[Double].
    def double2: Rand[Double] =
      examples.map(nonNegativeInt)(_.toDouble / Int.MaxValue)

    // 6.6
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)
        (f(a, b), rng3)
      }

    def intDouble2: Rand[(Int, Double)] =
      map2(nonNegativeInt, double2)((_, _))

    def doubleInt2: Rand[(Double, Int)] =
      map2(double2, nonNegativeInt)((_, _))

    // 6.7
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      rng => {
        fs.foldLeft((Nil: List[A], rng))((res, r) => {
          val n = r(res._2)
          (res._1 :+ n._1, n._2)
        })
      }

    def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      // This could actually be a for comprehension
      // if I somehow implemented it as methods of Rand.
      flatMap(ra) { a =>
        mapViaFlatMap(rb) {
          b => f(a, b)
        }
      }
    }

    def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s) { i =>
        examples.unit(f(i))
      }

    // 6.8
    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, rng2) = f(rng)
        g(a)(rng2)
      }
  }

  exercises.nonNegativeInt(simpleRng)
  exercises.double(simpleRng)
  exercises.intDouble(simpleRng)
  exercises.doubleInt(simpleRng)
  exercises.double3(simpleRng)
  exercises.ints(10)(simpleRng)
  exercises.double2(simpleRng)
  exercises.intDouble2(simpleRng)
  exercises.doubleInt2(simpleRng)
  exercises.sequence(List(examples.unit(1), examples.unit(2), examples.unit(3)))(simpleRng)
  examples.nonNegativeLessThan(6)(simpleRng)
  exercises.mapViaFlatMap(examples.unit(28))(_ - 5)(simpleRng)
  exercises.map2ViaFlatMap(examples.unit(0), examples.unit(9))(_ + _)(simpleRng)
  examples.rollDie(simpleRng)
  examples.mapViaGeneralMap(examples.unit(28))(_ - 5)(simpleRng)
}

