package com.algorithms

import com.fpinscala.testing.{Gen, PropertyTesting, SGen}
import org.p99.scala.UnitSpec
import org.scalatest.Matchers

/**
 * Created by korneliusz on 06.01.15.
 */
class LevenshteinDistanceSpec extends UnitSpec with Matchers with PropertyTesting {
  val strings = Gen.string()
  val stringPairs = strings.zipWith(strings)

  def identical[A](a1: SGen[A]): SGen[(A, A)] = a1.map(a => (a, a))

  it should "detect the distance for a simple case" in {
    LevenshteinDistance.naiveCompute("kitten", "sitting") should be(3)
    LevenshteinDistance.compute("kitten", "sitting") should be(3)
  }

  it should "be at least the difference of the sizes of the two strings" in {
    testProperty(Gen.forAll(stringPairs){p =>
      LevenshteinDistance.compute(p._1, p._2) >= Math.abs(p._1.length - p._2.length)
    })
  }

  it should "be at most the length of the longer string" in {
    testProperty(Gen.forAll(stringPairs) { p =>
      LevenshteinDistance.compute(p._1, p._2) <= p._1.length.max(p._2.length)
    })
  }

  it should "be zero if and only if the strings are equal" in {
    val allIdentical = Gen.forAll(identical(strings)) { p =>
      LevenshteinDistance.compute(p._1, p._2) == 0
    }

    val possiblyIdentical = Gen.forAll(stringPairs) { p =>
      val distance = LevenshteinDistance.compute(p._1, p._2)
      if (p._1 == p._2) distance == 0
      else distance != 0
    }

    testProperty(allIdentical && possiblyIdentical)
  }
}
