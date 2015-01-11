package com.algorithms

import com.fpinscala.testing.{Gen, PropertyTesting}
import org.p99.scala.UnitSpec

import scala.util.Try

/**
 * Created by elkorn on 1/11/15.
 */
class ContinuationSpec extends UnitSpec with PropertyTesting {
  "squaring numbers" should "be possible to do with continuations" in {
    def square(n: Int) = n * n
    def squareCont[R](n: Int) = Continuation.unit[R](square(n))

    testProperty(Gen.forAll(Gen.choose(0, 100)) { n =>
      squareCont(n)(x => x) == square(n)
    })
  }

  "division" should "be possible to do with continuations" in {
    def divCont[R](continue: String => Continuation[R, Int])(num: Int, den: Int): Continuation[R, Int] =
      Continuation.bind[R, Int, String](ok => {
        Continuation.bind[R, String, String](error => {
          if (den == 0) error("Denominator is 0.")
          else ok(num / den)
        })
      } flatMap continue)

    def divError[R]: (Int, Int) => Continuation[R, Int] = divCont[R](sys.error(_)) _

    testProperty(Gen.forAll(Gen.choose(1, 10).zipWith(Gen.choose(1, 10))) { pair =>
      divError(pair._1, pair._2)(x => x) == pair._1 / pair._2
    })

    testProperty(Gen.forAll(Gen.choose(1, 10).zipWith(Gen.choose(0, 1))) { pair =>
      Try(divError(pair._1, pair._2)(x => x)).isFailure
    })
  }
}
