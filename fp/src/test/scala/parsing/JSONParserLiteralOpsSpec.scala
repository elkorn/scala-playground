package fp.parsing

import org.scalatest._

class JSONParserLiteralOpsSpec extends FlatSpec with Matchers with EitherOps {
  import fp.parsing.JSON._
  val P = new MyParsers {}
  import P.asStringParser
  import P.operators

  val jsonP = JSONParserLiteralOps(P)
  import jsonP._

  "whitespace" should "omit any whitespace" in {
    whitespace.flatMap(_ => P.string("abra")).run("  \t \n abra") shouldSucceedWith ("abra")
    whitespace.flatMap(_ => P.string("abra")).run("  \t \n abra    \t \n") shouldSucceedWith ("abra")
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
    jNumber.run("2 3. 000") shouldFail
  }

  "jArray" should "recognize arrays" in {
    jArray.run("[1,2,3]") shouldSucceedWith JArray(List(JNumber(1.0), JNumber(2.0), JNumber(3.0)).toIndexedSeq)
  }
}
