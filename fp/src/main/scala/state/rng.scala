package fp.state

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  private def lcgSeed(seed: Long) = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFL

  def nextInt(): (Int, RNG) = {
    val newSeed = lcgSeed(seed)
    val newRng = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, newRng)
  }
}

object RNG {
  // Just making the concept explicit.
  type State[State, +Result] = State => (Result, State)
  // This type denotes a state transition of a random generator.
  // State transitions can also be called state actions.
  type Rand[+A] = State[RNG, A]

  def randomPair(rng: RNG): (Int, Int) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    (i1, i2)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, newRng) = rng.nextInt
    val result =
      math.abs(i match {
        case Int.MinValue => Int.MinValue + 1
        case x => x
      })

    (result, newRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = rng.nextInt
    val result = (i % Int.MaxValue).toDouble / Int.MaxValue
    (result, rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    var intermediate = rng
    (List.tabulate(count)((_) => {
      val (i, inter) = rng.nextInt
      intermediate = inter
      i
    }), intermediate)

  }

  def boolean: Rand[Boolean] = rng => {
    val (a, rng2) = rng.nextInt
    (a % 2 == 0, rng2)
  }

  def unit[A](a: A): Rand[A] =
    (rng) => (a, rng)

  def map[A, B](r: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = r(rng)
      (f(a), rng2)
    }

  val double2: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng1 => {
      val (a, rng2) = ra(rng1)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def int: Rand[Int] = _.nextInt

  def intDouble2: Rand[(Int, Double)] = both(int, double)
  def doubleInt2: Rand[(Double, Int)] = both(double, int)

  def sequence[A](ras: List[Rand[A]]): Rand[List[A]] =
    rng => ras.foldRight((Nil: List[A], rng))((ra, p) => {
      val (a, rng2) = ra(p._2)
      (a :: p._1, rng2)
    })

  def sequence2[A](ras: List[Rand[A]]): Rand[List[A]] = {
    ras.foldRight(unit(List[A]()))((a, r) => map2(a, r)(_ :: _))
  }

  def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng1 => {
      val (a, rng2) = ra(rng1)
      f(a)(rng2)
    }

  def map_2[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)((x) => unit(f(x)))

  // This should be a for-comprehension, but we did not yet get to typeclasses.
  def map2_2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
}
