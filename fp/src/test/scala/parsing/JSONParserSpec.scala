package fp.parsing

import org.scalatest._

class JSONParserSpec extends FlatSpec with Matchers with EitherOps {
  import fp.parsing.JSON._
  val P = new MyParsers {}
  val jsonP = jsonParser(P)

  // "whitespace" should "omit any whitespace" in {
  //   val input = "[1,2,3]"

  //   P.run(jsonP)(input) shouldSucceedWith (JArray(List(JNumber(1.0), JNumber(2.0), JNumber(3.0)).toIndexedSeq))
  // }
}
