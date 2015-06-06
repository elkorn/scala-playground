package fp.property.domain

import org.scalatest._
import fp.Lazy.Stream

class FiniteDomainSpec extends FlatSpec with Matchers {
  import Domain._

  "FiniteDomain" should "accept a list" in {
    // It works if it compiles.
    FiniteDomain(List(1, 2, 3))
  }

  "FiniteDomain" should "be finite" in {
    val domain = FiniteDomain(List(1))
    domain.isFinite should be(true)
    domain.tail.isFinite should be(true)
  }

  "FiniteDomain" should "be exhaustible" in {
    val domain = FiniteDomain(List(1))
    domain.isExhausted should be(false)
    domain.tail.isExhausted should be(true)
    a[java.util.NoSuchElementException] should be thrownBy domain.tail.head()
  }

  "FiniteDomain" should "have size information" in {
    FiniteDomain(List(1, 2, 3)).size should equal(3)
    FiniteDomain(List(1, 2, 3)).tail.size should equal(2)
  }

  "FiniteDomain" should "be mappable" in {
    FiniteDomain(List(1, 2, 3)).map(_ + 12).head() should equal(13)
  }

  "FiniteDomain" should "be flatmappable" in {
    val mapped = FiniteDomain(List(1, 2, 3)).flatMap(x => FiniteDomain(List(x + 12)))
    mapped head () should equal(13)
    mapped.tail.head() should equal(14)
    mapped.tail.tail.head() should equal(15)
  }

  "FiniteDomain" should "have a cartesian product" in {
    def deepToList[A](domain: FiniteDomain[Stream[A]]): List[List[A]] = domain.as.map(_.toList).toList
    val input = FiniteDomain(List(
      Stream(1, 2),
      Stream(3, 4),
      Stream(5, 6)
    ))

    val expected = FiniteDomain(List(
      Stream(1, 3, 5),
      Stream(1, 3, 6),
      Stream(1, 4, 5),
      Stream(1, 4, 6),
      Stream(2, 3, 5),
      Stream(2, 3, 6),
      Stream(2, 4, 5),
      Stream(2, 4, 6)
    ))

    val actual: FiniteDomain[Stream[Int]] = Domain.cartesian(input).finite

    deepToList(actual) should equal(deepToList(expected))
  }

  "FiniteDomain" should "be extractable" in {
    // It works if it compiles.
    val domain = FiniteDomain(List(1, 2, 3))

    domain match {
      case FiniteDomain(h, tailDomain) => h().isInstanceOf[Int] should be(true)
    }
  }

  "FiniteDomain" should "recognize empty tails" in {
    // It works if it compiles.
    val domain = FiniteDomain(List(1, 2))

    domain match {
      case FiniteDomain(h, EmptyDomain) => fail("Recognized a non-empty domain as empty")
      case _ =>
    }

    domain.tail match {
      case FiniteDomain(h, EmptyDomain) =>
      case _ => fail("Did not recognize an empty domain")
    }
  }
}
