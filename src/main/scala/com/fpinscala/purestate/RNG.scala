package com.fpinscala.purestate


trait RNG {
  /**
   * Returning the state that would be mutated in place in OOP is a general pattern in pure FP.
   * @return a random number and the new internal state of the generator.
   */
  def nextInt: (Int, RNG)

  def nonNegativeInt(): (Int, RNG) = {
    val (nextInt, rng2) = this.nextInt
    if (nextInt == Int.MinValue) rng2.nonNegativeInt
    else (nextInt.abs, rng2)
  }
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      // http://en.wikipedia.org/wiki/Linear_congruential_generator
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }

  def nonNegativeInt(seed: Long = 1): (Int, RNG) = simple(seed).nonNegativeInt
}