package fp.parsing

import org.scalatest._

class MyParserSpec extends FlatSpec with Matchers with EitherOps {
  val P = new MyParsers {}
  import P._
  import P.asStringParser
  import P.operators

  "string" should "detect if the input starts with a string" in {
    string("abra").run("abracadabra") shouldSucceedWith ("abra")
    string("abra").run("cabracadabra") shouldFail
  }

  "char" should "detect if the input starts with a character" in {
    char('a').run("abracadabra") shouldSucceedWith ('a')
    char('b').run("abracadabra") shouldFail
  }

  "regex" should "detect parts of the input that match a regexp" in {
    P.regex("(abra)+".r).run("abraabraabra") shouldSucceedWith ("abraabraabra")
    P.regex("(abra)+".r).run("abracadabraabraabra") shouldSucceedWith ("abra")
    P.regex("(abra)+".r).run("cadabraabraabra") shouldFail
  }

  "flatMap" should "apply a mapping fn and flatten the result" in {
    flatMap(string("1"))(_ => string("3")).run("13") shouldSucceedWith ("3")
    flatMap(string("1"))(_ => char('3')).run("13") shouldSucceedWith ('3')
  }

  "or" should "create an alternative of parsers" in {
    val p = string("abra") | string("cadabra")

    p.run("abracadabra") shouldSucceedWith ("abra")
    p.run("cadabrabra") shouldSucceedWith ("cadabra")
  }

  "or" should "not try out the right branch if the left one has been commited to" in {
    val p = P.fail() | string("abra")
    p.run("abracadabra") shouldFail
  }

  "many" should "detect a list of matches" in {
    val p = string("abra") | string("cadabra")

    p.many.run("abracadabra") shouldSucceedWith (List("abra", "cadabra"))
    p.many.run("cadabrabra") shouldSucceedWith (List("cadabra"))
    p.many.run("cadabraabraabra") shouldSucceedWith (List("cadabra", "abra", "abra"))
  }

  "succeed" should "create an always successful parser" in {
    P.succeed(42).run("agpidahgpiadg") shouldSucceedWith (42)
  }

  "fail" should "create an always failing parser" in {
    P.fail().run("agpidahgpiadg") shouldFail
  }

  "surround" should "detect matches surrounded with specified other matches" in {
    string("test").surround(char('['), char(']')).run("[test]") shouldSucceedWith ("test")
    string("test").surround(char('['), char(']')).run("{test}") shouldFail
  }
}
