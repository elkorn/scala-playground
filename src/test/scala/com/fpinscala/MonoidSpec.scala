package com.fpinscala

import com.fpinscala.monoids.Monoid
import com.fpinscala.testing.{Gen, PropertyTesting}
import org.p99.scala.UnitSpec
import org.scalatest.Matchers

/**
 * Created by elkorn on 1/20/15.
 */
class MonoidSpec extends UnitSpec with Matchers with PropertyTesting {
  "foldMap" should "transform elements and fold them using the monoid" in {
    def sum2(a: Int, b: Int) = a + b + 2
    def sumM = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 + a2

      override def zero: Int = 0
    }

    testProperty(Gen.forAll(Gen.listOf(Gen.choose(-10, 10))) { list =>
      Monoid.foldMap(list, sumM)(_ + 2) == list.foldLeft(0)(sum2)
    })
  }

}
