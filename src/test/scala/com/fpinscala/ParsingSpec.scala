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
      Parsers.char(c).run(str) == Right(str)
    }
    })
  }

  "string parser" should "be able to parse strings" in {
    testProperty(Gen.forAll(Gen.string()) { str => {
      Parsers.string(str).run(str) == Right(str)
    }
    })
  }

  "or-string parser" should "be able to parse two strings" in {
    val strings = Gen.string()
    val stringPairs = strings.zipWith(strings)
    testProperty(Gen.forAll(stringPairs) { pair => {
      Parsers.orString(pair._1, pair._2).run(pair._1) == Right(pair._1)
      Parsers.orString(pair._1, pair._2).run(pair._2) == Right(pair._2)
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
      (pair._1 | pair._2).run(pair._1) == Right(pair._1)
      (pair._1 | pair._2).run(pair._2) == Right(pair._2)
    }
    })
  }

  "ListOfN combinator" should "recognize repetitions" in {
    // These two lines enable the infix syntax.
    val P: Parsers
    import P._

    val strings = Gen.string()
    val stringPairs = strings.zipWith(strings)
    Parsers.listOfN(3, "ab" | "cad").run("ababcad") should be(Right("ababcad"))
    Parsers.listOfN(3, "ab" | "cad").run("cadabab") should be(Right("cadabab"))
    Parsers.listOfN(3, "ab" | "cad").run("ababab") should be(Right("ababab"))
  }

  "counting parser" should "count the number of given characters" in {
    Parsers.countChar("a").run("aaaa") should be(Right(4))
    Parsers.countChar("a").run("") should be(Right(0))
    Parsers.countChar("a").run("b") should be(Right(0))
  }

  "slice combinator" should "show the examined part of the input string" in {
    val P: Parsers
    import P._

    (slice(('a' | 'b').many)).run("aaba") should be(Right("aaba"))


    testProperty(Gen.forAll(Gen.string()) { str =>
      (slice(str).many).run(str) == Right(str)
    })
  }

  "listOfN1" should "be equivalent to the book implementation" in {
    val n = 5
    testProperty(Gen.forAll(Gen.string()) { str =>
      val char: Any = Parsers.char(str.charAt(0))
      Parsers.Laws.equal(Parsers.listOfN(n, char), Parsers.listOfN1(n, char))(str)
    })
  }

  "listOfN2" should "be equivalent to the book implementation" in {
    val n = 5
    testProperty(Gen.forAll(Gen.string()) { str =>
      val char: Any = Parsers.char(str.charAt(0))
      Parsers.Laws.equal(Parsers.listOfN(n, char), Parsers.listOfN2(n, char))(str)
    })
  }

  "contextual" should "discover the number of characters that should follow a number" in {
    testProperty(Gen.forAll(Gen.choose(1, 100).zipWith(Gen.char())) {
      case (n, ch) => {
        val correctPattern = s"$n${List.fill(n)(ch).mkString}"
        Parsers.contextual(Parsers.char(ch)).run(correctPattern) == Right(correctPattern)
      }
    })
  }
}
