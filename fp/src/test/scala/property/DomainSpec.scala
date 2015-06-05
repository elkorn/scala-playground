package fp.property

import org.scalatest._
import fp.Lazy.Stream

class DomainSpec extends FlatSpec with Matchers {
  import Domain._

  "FiniteDomain" should "accept a list" in {
    // It works if it compiles.
    new FiniteDomain(List(1, 2, 3))
  }

  "FiniteDomain" should "be finite" in {
    val domain = new FiniteDomain(List(1))
    domain.isFinite should be(true)
    domain.tail.isFinite should be(true)
  }

  "FiniteDomain" should "be exhaustible" in {
    val domain = new FiniteDomain(List(1))
    domain.isExhausted should be(false)
    domain.tail.isExhausted should be(true)
    a[java.util.NoSuchElementException] should be thrownBy domain.tail.head()
  }

  "FiniteDomain" should "have size information" in {
    new FiniteDomain(List(1, 2, 3)).size should equal(3)
    new FiniteDomain(List(1, 2, 3)).tail.size should equal(2)
  }

  "InfiniteDomain" should "accept a stream" in {
    // It works if it compiles.
    new InfiniteDomain(Stream.from(0))
  }

  "InfiniteDomain" should "be infinite" in {
    var domain = new InfiniteDomain(Stream.from(0))
    (1 to 100) foreach { _ =>
      domain.isFinite should be(false)
      domain = domain.tail
    }
  }

  "InfiniteDomain" should "not be exhaustible" in {
    var domain = new InfiniteDomain(Stream.from(0))
    (1 to 100) foreach { _ =>
      domain.isExhausted should be(false)
      domain = domain.tail
    }
  }

  "InfiniteDomain" should "throw errors when built on finite streams" in {
    val domain = new InfiniteDomain(Stream(1))
    a[RuntimeException] should be thrownBy (domain.tail)
  }

  "Domain" should "be extractable" in {
    // It works if it compiles.
    val domain = new InfiniteDomain(Stream.from(0))

    domain match {
      case Domain(h, tail) =>
      case _ => fail("???")
    }
  }

  "FiniteDomain" should "be extractable" in {
    // It works if it compiles.
    val domain = new FiniteDomain(List(1, 2, 3))

    domain match {
      case FiniteDomain(h, tailDomain) =>
      case _ => fail("???")
    }
  }

  "FiniteDomain" should "recognize empty tails" in {
    // It works if it compiles.
    val domain = new FiniteDomain(List(1, 2))

    domain match {
      case FiniteDomain(h, EmptyDomain) => fail("Recognized a non-empty domain as empty")
      case _ =>
    }

    domain.tail match {
      case FiniteDomain(h, EmptyDomain) =>
      case _ => fail("Did not recognize an empty domain")
    }
  }

  "InfiniteDomain" should "be extractable" in {
    // It works if it compiles.
    val domain = new InfiniteDomain(Stream.from(0))

    domain match {
      case InfiniteDomain(h, tailDomain) =>
      case _ => fail("???")
    }
  }
}
