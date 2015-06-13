package fp.parsing

import org.scalatest._

class MyParserSpec extends FlatSpec with Matchers {
  val P = new MyParsers {}
  import P.asStringParser
  import P.operators

  implicit def ops[A, B](either: Either[A, B]) = EitherOps(either)

  case class EitherOps[A, B](either: Either[A, B]) {
    def shouldFail = either shouldBe a[Left[_, _]]

    def shouldSucceedWith(expected: B) = either should equal(Right(expected))
  }

  "string" should "detect if the input starts with a string" in {
    P.run(P.string("abra"))("abracadabra") shouldSucceedWith ("abra")
    P.run(P.string("abra"))("cabracadabra") shouldFail
  }

  "char" should "detect if the input starts with a character" in {
    P.run(P.char('a'))("abracadabra") shouldSucceedWith ('a')
    P.run(P.char('b'))("abracadabra") shouldFail
  }

  "regex" should "detect parts of the input that match a regexp" in {
    P.run(P.regex("(abra)+".r))("abraabraabra") shouldSucceedWith ("abraabraabra")
    P.run(P.regex("(abra)+".r))("abracadabraabraabra") shouldSucceedWith ("abra")
    P.run(P.regex("(abra)+".r))("cadabraabraabra") shouldFail
  }

  "flatMap" should "apply a mapping fn and flatten the result" in {
    P.run(P.flatMap(P.string("1"))(_ => P.string("3")))("13") shouldSucceedWith ("3")
    P.run(P.flatMap(P.string("1"))(_ => P.char('3')))("13") shouldSucceedWith ('3')
  }

  "or" should "create an alternative of parsers" in {
    val p = P.string("abra") | P.string("cadabra")

    P.run(p)("abracadabra") shouldSucceedWith ("abra")
    P.run(p)("cadabrabra") shouldSucceedWith ("cadabra")
  }

  "or" should "not try out the right branch if the left one has been commited to" in {
    val p = P.fail() | P.string("abra")
    P.run(p)("abracadabra") shouldFail
  }

  "many" should "detect a list of matches" in {
    val p = P.string("abra") | P.string("cadabra")

    P.run(p many)("abracadabra") shouldSucceedWith (List("abra", "cadabra"))
    P.run(p many)("cadabrabra") shouldSucceedWith (List("cadabra"))
    P.run(p many)("cadabraabraabra") shouldSucceedWith (List("cadabra", "abra", "abra"))
  }

  "succeed" should "create an always successful parser" in {
    P.run(P.succeed(42))("agpidahgpiadg") shouldSucceedWith (42)
  }

  "fail" should "create an always failing parser" in {
    P.run(P.fail())("agpidahgpiadg") shouldFail
  }

  "surround" should "detect matches surrounded with specified other matches" in {
    P.run(P.string("test").surround(P.char('['), P.char(']')))("[test]") shouldSucceedWith ("test")
    P.run(P.string("test").surround(P.char('['), P.char(']')))("{test}") shouldFail
  }
}
