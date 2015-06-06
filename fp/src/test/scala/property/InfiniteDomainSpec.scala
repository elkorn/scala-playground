package fp.property.domain

import org.scalatest._
import fp.Lazy.Stream

class InfiniteDomainSpec extends FlatSpec with Matchers {
  import Domain._

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
    def deepToList[A](s: Stream[Stream[A]]): List[List[A]] = s.map(_.toList).toList
    val s = InfiniteDomain(
      Stream(
        Stream(1, 2),
        Stream(3, 4),
        Stream(5, 6)
      )
    )
    val expected = Stream(Stream(1, 3, 5), Stream(1, 3, 6), Stream(1, 4, 5), Stream(1, 4, 6), Stream(2, 3, 5), Stream(2, 3, 6), Stream(2, 4, 5), Stream(2, 4, 6))

    val actual = Domain.cartesian(s).as.take(8).map(_.take(3))

    deepToList(actual) should equal(deepToList(expected))
  }

  "InfiniteDomain" should "be extractable" in {
    // It works if it compiles.
    val domain = new InfiniteDomain(Stream.from(0))

    domain match {
      case InfiniteDomain(h, tailDomain) =>
    }
  }
}
