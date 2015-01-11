package com.algorithms

object LevenshteinDistance {
  def compute(s1: String, s2: String): Int = {
    lazy val distance: Memoize[(Int, Int), Int] = Memoize(p => go(s1, p._1, s2, p._2))

    def go(s1: String, n1: Int, s2: String, n2: Int): Int = {
      if (n1 == 0) return n2
      else if (n2 == 0) return n1

      val cost = if (s1.charAt(n1 - 1) == s2.charAt(n2 - 1)) 0 else 1
      (distance(n1 - 1, n2) + 1)
        .min(distance(n1, n2 - 1) + 1)
        .min(distance(n1 - 1, n2 - 1) + cost)
    }

    go(s1, s1.length, s2, s2.length)
  }

  def naiveCompute(s1: String, s2: String): Int = {
    def go(s1: String, n1: Int, s2: String, n2: Int): Int = {
      if (n1 == 0) return n2
      else if (n2 == 0) return n1

      val cost = if (s1.charAt(n1 - 1) == s2.charAt(n2 - 1)) 0 else 1
      (go(s1, n1 - 1, s2, n2) + 1)
        .min(go(s1, n1, s2, n2 - 1) + 1)
        .min(go(s1, n1 - 1, s2, n2 - 1) + cost)
    }

    go(s1, s1.length, s2, s2.length)
  }

}
