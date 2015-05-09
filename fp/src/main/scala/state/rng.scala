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
}
