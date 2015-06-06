package fp.property.domain

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

  "FiniteDomain" should "be mappable" in {
    new FiniteDomain(List(1, 2, 3)).map(_ + 12).head() should equal(13)
  }

  "FiniteDomain" should "be flatmappable" in {
    val mapped = new FiniteDomain(List(1, 2, 3)).flatMap(x => FiniteDomain(x + 12))
    mapped head () should equal(13)
    mapped.tail.head() should equal(14)
    mapped.tail.tail.head() should equal(15)
  }

  "FiniteDomain" should "have a cartesian product" in {
    val input = FiniteDomain(
      FiniteDomain(1, 2),
      FiniteDomain(3, 4),
      FiniteDomain(5, 6)
    )
    val expected = FiniteDomain(
      FiniteDomain(1, 3, 5),
      FiniteDomain(1, 3, 6),
      FiniteDomain(1, 4, 5),
      FiniteDomain(1, 4, 6),
      FiniteDomain(2, 3, 5),
      FiniteDomain(2, 3, 6),
      FiniteDomain(2, 4, 5),
      FiniteDomain(2, 4, 6)
    )

    def deepToList[A](domain: FiniteDomain[FiniteDomain[A]]) = domain.as.map(_.as)

    deepToList(FiniteDomain.cartesian(input)) should equal(deepToList(expected))
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

  "InfiniteDomain" should "be mappable" in {
    new InfiniteDomain(Stream.from(0)).map(_ + 12).head() should equal(12)
  }

  "InfiniteDomain" should "be flatmappable" in {
    val mapped = new InfiniteDomain(Stream.from(1)).flatMap(x => InfiniteDomain(Stream(x + 12)))
    mapped head () should equal(13)
    mapped.tail.head() should equal(14)
    mapped.tail.tail.head() should equal(15)
  }

  "InfiniteDomain" should "throw errors when built on finite streams" in {
    val domain = new InfiniteDomain(Stream(1))
    a[RuntimeException] should be thrownBy (domain.tail)
  }

  "InfiniteDomain" should "have a cartesian product" in {
    import fp.Lazy.Stream
    def deepToList[A](s: Stream[Stream[A]]): List[List[A]] =
      s.map(_.toList).toList
    val s = InfiniteDomain(
      Stream(
        InfiniteDomain(Stream(1, 2)),
        InfiniteDomain(Stream(3, 4)),
        InfiniteDomain(Stream(5, 6))
      )
    )
    val expected = Stream(Stream(1, 3, 5), Stream(1, 3, 6), Stream(1, 4, 5), Stream(1, 4, 6), Stream(2, 3, 5), Stream(2, 3, 6), Stream(2, 4, 5), Stream(2, 4, 6))

    val actual = InfiniteDomain.cartesian(s).as.take(8).map(_.as.take(3))

    deepToList(actual) should equal(deepToList(expected))
  }

  "Domain" should "be extractable" in {
    // It works if it compiles.
    val domain = new InfiniteDomain(Stream.from(0))

    domain match {
      case Domain(h, tail) =>
    }
  }

  "FiniteDomain" should "be extractable" in {
    // It works if it compiles.
    val domain = new FiniteDomain(List(1, 2, 3))

    domain match {
      case FiniteDomain(h, tailDomain) => h().isInstanceOf[Int] should be(true)
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
    }
  }
}
