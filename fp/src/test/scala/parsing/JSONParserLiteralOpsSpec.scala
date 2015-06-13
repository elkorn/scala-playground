package fp.parsing

import org.scalatest._

class JSONParserLiteralOpsSpec extends FlatSpec with Matchers with EitherOps {
  import fp.parsing.JSON._
  val P = new MyParsers {}
  import P.operators

  val jsonP = JSONParserLiteralOps(P)
  import jsonP._

  "whitespace" should "omit any whitespace" in {
    P.whitespace.flatMap(_ => P.string("abra")).run("  \t \n abra") shouldSucceedWith ("abra")
    P.whitespace.flatMap(_ => P.string("abra")).run("  \t \n abra    \t \n") shouldSucceedWith ("abra")
  }

  "jStr" should "recognize quoted strings" in {
    jStr.run("'test'") shouldSucceedWith JString("test")
    jStr.run("\"test\"") shouldSucceedWith JString("test")
    jStr.run("\"t e s 13059713 !#%!#GADJGKAJGt\"") shouldSucceedWith JString("t e s 13059713 !#%!#GADJGKAJGt")
    jStr.run("test") shouldFail
  }

  "jNumber" should "recognize numbers" in {
    jNumber.run("2") shouldSucceedWith JNumber(2.0)
    jNumber.run("2.124") shouldSucceedWith JNumber(2.124)
    jNumber.run("2.000") shouldSucceedWith JNumber(2.0)
    jNumber.run("2,000") shouldSucceedWith JNumber(2.0)
    jNumber.run("-2") shouldSucceedWith JNumber(-2.0)
    jNumber.run("2 3. 000") shouldFail
  }

  "jArray" should "recognize arrays" in {
    jArray.run("[1,2,3]") shouldSucceedWith JArray(List(JNumber(1.0), JNumber(2.0), JNumber(3.0)).toIndexedSeq)
    jArray.run("""[1,2,"3"]""") shouldSucceedWith JArray(List(JNumber(1.0), JNumber(2.0), JString("3")).toIndexedSeq)
    jArray.run("""[1,2,[3,4]]""") shouldSucceedWith JArray(List(JNumber(1.0), JNumber(2.0), JArray(List(JNumber(3.0), JNumber(4.0)).toIndexedSeq)).toIndexedSeq)
  }

  "kv" should "recognize key-value pairs" in {
    kv.run(""""a": 12""") shouldSucceedWith (("a", JNumber(12)))
    kv.run("""'a': 12""") shouldSucceedWith (("a", JNumber(12)))
    kv.run("""     'a': 12    """) shouldSucceedWith (("a", JNumber(12)))
    kv.run("""     'a'     :     12    """) shouldSucceedWith (("a", JNumber(12)))
  }

  "jObj" should "recognize objects" in {
    val obj1 = """{
      "a": 12,
      "b": "hello"
  }"""
    val obj2 = """{
      "a": 12,
      "b": ["hello", 123]
  }"""
    val obj3 = """{
      "a": 12,
      "b": { "hello": 123 }
  }"""

    val trailing = """{
      "a": 12,
      "b": { "hello": 123 }
  }    123"""

    jObj.run(obj1) shouldSucceedWith JObject(Map("a" -> JNumber(12.0), "b" -> JString("hello")))
    jObj.run(obj2) shouldSucceedWith JObject(Map("a" -> JNumber(12.0), "b" -> JArray(List(JString("hello"), JNumber(123.0)).toIndexedSeq)))
    jObj.run(obj3) shouldSucceedWith JObject(Map("a" -> JNumber(12.0), "b" -> JObject(Map("hello" -> JNumber(123.0)))))
  }
}
