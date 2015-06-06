package fp.property.domain

import org.scalatest._
import fp.Lazy.Stream

class DomainSpec extends FlatSpec with Matchers {
  "Domain" should "be extractable" in {
    // It works if it compiles.
    val domain = new InfiniteDomain(Stream.from(0))

    domain match {
      case Domain(h, tail) =>
    }
  }
}
