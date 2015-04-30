package fp.errors

import org.scalatest._

class OptionSpec extends FlatSpec with Matchers {
  "map" should "apply a mapping function" in {
    Some(5).map(_.toString) should equal(Some("5"))
    None.map(_.toString) should equal(None)
  }

  "getOrElse" should "get the value for Some" in {
    Some(5).getOrElse("") should equal(5)
  }

  "getOrElse" should "get the default for None" in {
    None.getOrElse(5) should equal(5)
  }

  "flatMap" should "apply the mapping fn and flatten the result" in {
    Some(5).flatMap((x) => Some(x.toString)) should equal(Some("5"))
    Some(5).flatMap((_) => None) should equal(None)
    None.flatMap((x) => Some(x.toString)) should equal(None)
  }

  "orElse" should "get the value for Some" in {
    Some(5).orElse(Some("")) should equal(Some(5))
    Some(5).orElse(None) should equal(Some(5))
  }

  "orElse" should "get the default for None" in {
    None.orElse(Some(5)) should equal(Some(5))
    None.orElse(None) should equal(None)
  }
}
