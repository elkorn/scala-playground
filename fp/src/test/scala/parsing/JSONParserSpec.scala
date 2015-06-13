package fp.parsing

import org.scalatest._

class JSONParserSpec extends FlatSpec with Matchers with EitherOps {
  import fp.parsing.JSON._
  val P = new MyParsers {}
  import P.operators

  val jsonP = JSONParserLiteralOps(P)
  import jsonP._
  val parser = jsonParser(P)

  "jsonParser" should "parse valid JSON" in {
    val obj3 = """{
      "a": 12,
      "b": { "hello": 123 }
  }"""

    parser.run(obj3) shouldSucceedWith JObject(Map("a" -> JNumber(12.0), "b" -> JObject(Map("hello" -> JNumber(123.0)))))
  }

  "jsonParser" should "expect no trailing characters" in {
    val trailing = """{
      "a": 12,
      "b": { "hello": 123 }
  }   123"""

    parser.run(trailing) shouldFail ()
  }
}
