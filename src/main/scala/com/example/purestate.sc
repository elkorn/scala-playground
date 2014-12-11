import scala.annotation.tailrec
object purestate {
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
  val simpleRng: RNG = RNG.simple(2)

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
  }

  object exercises {
    // 6.1
    def positiveInt(rng: RNG): (Int, RNG) = {
      val (nextInt, rng2) = rng.nextInt
      if (nextInt == Int.MinValue) positiveInt(rng2)
      else (nextInt.abs, rng2)
    }

    // 6.2
    def positiveDouble(rng: RNG): (Double, RNG) = {
      val (result, rng2) = positiveInt(rng)
      (result.toDouble / Int.MaxValue, rng2)
    }

    // 6.3
    def positiveIntDouble(rng: RNG): ((Int, Double), RNG) = {
      val (r1, rng2) = positiveInt(rng)
      val (r2, rng3) = positiveDouble(rng2)

      ((r1, r2), rng3)
    }

    def positiveDoubleInt(rng: RNG): ((Double, Int), RNG) = {
      val (r1, rng2) = positiveDouble(rng)
      val (r2, rng3) = positiveInt(rng2)

      ((r1, r2), rng3)
    }

    def positiveDouble3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (r1, rng2) = positiveDouble(rng)
      val (r2, rng3) = positiveDouble(rng2)
      val (r3, rng4) = positiveDouble(rng3)

      ((r1, r2, r3), rng4)
    }

    // 6.4
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      @tailrec
      def go(result: (Int, RNG), acc: List[Int], remaining: Int): (List[Int], RNG) = {
        if (remaining == 0) (acc :+ result._1, result._2)
        else {
          go(result._2.nextInt, acc :+ result._1, remaining-1)
        }
      }

      go(rng.nextInt, Nil:List[Int], count)
    }
  }
  exercises.positiveInt(simpleRng)
  exercises.positiveDouble(simpleRng)
  exercises.positiveIntDouble(simpleRng)
  exercises.positiveDoubleInt(simpleRng)
  exercises.positiveDouble3(simpleRng)
  exercises.ints(10)(simpleRng)
}

