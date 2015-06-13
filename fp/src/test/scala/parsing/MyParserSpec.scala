package fp.parsing

import org.scalatest._
import org.scalamock.scalatest.MockFactory

class MyParserSpec extends FlatSpec with Matchers with MockFactory {
  val P = new MyParsers {}

  "run" should "run a parser against given input" in {

  }
}
