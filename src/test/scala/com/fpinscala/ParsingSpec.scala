package com.fpinscala

import com.fpinscala.parsing.Parsers
import com.fpinscala.testing.{Gen, PropertyTesting}
import org.p99.scala.UnitSpec
import org.scalatest.Matchers

/**
 * Created by elkorn on 1/10/15.
 */
class ParsingSpec extends UnitSpec with Matchers with PropertyTesting {
  "char parser" should "be able to parse chars" in {
    val chars = Gen.choose(0, 127).map(_.toChar)


    testProperty(Gen.forAll(chars) { c => {
      val str: String = c.toString
      Parsers.run(Parsers.char(c)(str)) == Right(str)
    }
    })
  }

  "string parser" should "be able to parse strings" in {
    testProperty(Gen.forAll(Gen.string()) { str => {
      Parsers.run(Parsers.string(str)(str)) == Right(str)
    }
    })
  }

  "or-string parser" should "be able to parse two strings" in {
    val strings = Gen.string()
    val stringPairs = strings.zipWith(strings)
    testProperty(Gen.forAll(stringPairs) { pair => {
      Parsers.run(Parsers.orString(pair._1, pair._2)(pair._1)) == Right(pair._1)
      Parsers.run(Parsers.orString(pair._1, pair._2)(pair._2)) == Right(pair._2)
    }
    })
  }

  "OR combinator" should "do an alternative operation" in {
    // These two lines enable the infix syntax.
    val P: Parsers
    import P._

    val strings = Gen.string()
    val stringPairs = strings.zipWith(strings)
    testProperty(Gen.forAll(stringPairs) { pair => {
      Parsers.run(pair._1 | pair._2)(pair._1) == Right(pair._1)
      Parsers.run(pair._1 | pair._2)(pair._2) == Right(pair._2)
    }
    })
  }

  "ListOfN combinator" should "recognize repetitions" in {
    // These two lines enable the infix syntax.
    val P: Parsers
    import P._

    val strings = Gen.string()
    val stringPairs = strings.zipWith(strings)
    (Parsers.run(Parsers.listOfN(3, "ab" | "cad"))("ababcad")) should be(Right("ababcad"))
    (Parsers.run(Parsers.listOfN(3, "ab" | "cad"))("cadabab")) should be(Right("cadabab"))
    (Parsers.run(Parsers.listOfN(3, "ab" | "cad"))("ababab")) should be(Right("ababab"))
  }

  "counting parser" should "count the number of given characters" in {
    (Parsers.run(Parsers.countChar("a"))("aaaa")) should be(Right(4))
    (Parsers.run(Parsers.countChar("a"))("")) should be(Right(0))
    (Parsers.run(Parsers.countChar("a"))("b")) should be(Right(0))
  }

}
