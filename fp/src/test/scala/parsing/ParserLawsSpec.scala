package fp.parsing

import org.scalatest._
import org.scalatest.prop._

class ParserLawsSpec extends FlatSpec with Matchers with PropertyChecks {
  "char" should "parse a single given char" in {
    forAll { (c: Char) =>
      // run(char(c))(c.toString) should equal(Right(c))
    }
  }

  "string" should "parse a given string" in {
    forAll { (s: String) =>
      // run(char(s))(s) should equal(Right(s))
    }
  }

  "or" should "create a parser conjunction" in {
    forAll { (s1: String, s2: String) =>
      // run(or( string(s1), string(s2) ))(s1) should equal(Right(s1))
      // run(or( string(s1), string(s2) ))(s2) should equal(Right(s2))
    }
  }

  "or" should "have an infix form" in {
    // Given P: Parsers,
    // import P._
    // allows to use the infix operators.

    forAll { (s1: String, s2: String) =>
      // run(string(s1) or string(s2))(s1) should equal(Right(s1))
      // run(string(s1) or string(s2))(s2) should equal(Right(s2))
    }
  }

  "|" should "be an infix alias for or" in {
    // Given P: Parsers,
    // import P._
    // allows to use the infix operators.
    forAll { (s1: String, s2: String) =>
      // run(string(s1) | string(s2))(s1) should equal(Right(s1))
      // run(string(s1) | string(s2))(s2) should equal(Right(s2))
    }
  }

  // TODO token concept.
  "repetitions" should "parse repeated sequences of tokens" in {
    forAll { (s1: String, s2: String) =>
      // run(repetitions(3, string( s1 )))(s1 ++ s1 ++ s1) should equal(Right(s1 ++ s1 ++ s1))
      // run(repetitions(3, string(s1) | string(s2)))(s1 ++ s2 ++ s1) should equal(Right(s1 ++ s2 ++ s1))

      // TODO what about failures? How should the cases fail?
    }
  }

  "map" should "honor the identity law" in {
    def identity[A](a: A): A = a

    forAll { (s: String) =>
      //run(string(s).map(identity))(s) should equal(run(string(s))(s))
    }
  }

  "count" should "return the number of occurences of a token" in {
    forAll { (s: String) =>
      // if (s.isEmpty) Succeeded
      // else {
      //   val c = s(0)
      //   // run(count(char(c)))(s) should equal(Right( s.count(_==c)))
      // }
    }
  }
}
