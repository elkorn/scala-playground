package fp.property

import fp.state.SimpleRNG
import org.scalatest._

class UsageSpec extends FlatSpec with Matchers {
  import Gen._
  import fp.property.domain.{
    Domain,
    FiniteDomain,
    InfiniteDomain
  }

  val domain = choose(-10, 10)
  "max" should "return the maximum value from a list" in {
    val maxProp = forAll(nonEmptyListOf(domain)) { ns =>
      {
        val max = ns.max
        !ns.exists(_ > max)
      }
    }

    Prop.check(maxProp) should equal(Result.Exhausted)
  }

  "sorted" should "return an ordered list" in {
    val maxProp = forAll(nonEmptyListOf(domain)) { ns =>
      {
        val sorted = ns.sorted
        val a =
          (ns.isEmpty || sorted.tail.isEmpty || !sorted.zip(sorted.tail).exists {
            case (a, b) => a > b
          })
        val b = !ns.exists(!sorted.contains(_))
        val c = !sorted.exists(!ns.contains(_))

        a && b && c
      }
    }

    Prop.check(maxProp) should equal(Result.Exhausted)
  }

  "interleave" should "produce correct domain types" in {
    import fp.Lazy.Stream

    def ctrl(bs: Boolean*) =
      FiniteDomain(bs.toList)

    val rng = SimpleRNG(123L)

    val infinite1 = InfiniteDomain(Stream.from(1))
    val infinite2 = InfiniteDomain(Stream.from(6))
    val finite1 = infinite1.take(5)
    val finite2 = infinite2.take(5)

    val infiniteCtrl = InfiniteDomain(Stream.constant(true))
    val finiteCtrl = infiniteCtrl.take(5)

    def shouldBeFinite[A](domain: Domain[A], err: String) = domain match {
      case InfiniteDomain(_, _) => fail(err)
      case FiniteDomain(_, _) => // ok
      case x => fail(s"Unexpected result: $x")
    }

    def shouldBeInfinite[A](domain: Domain[A], err: String) = domain match {
      case FiniteDomain(_, _) => fail(err)
      case InfiniteDomain(_, _) => // ok
      case x => fail(s"Unexpected result: $x")
    }

    shouldBeFinite(
      Gen.interleave(finiteCtrl, finite1, finite2),
      "interleaving FINITE DOMAINS using a FINITE CONTROL DOMAIN should result in a FINITE DOMAIN"
    )

    shouldBeFinite(
      Gen.interleave(finiteCtrl, infinite1, infinite2),
      "interleaving INFINITE DOMAINS using a FINITE CONTROL DOMAIN should result in a FINITE DOMAIN"
    )

    shouldBeFinite(
      Gen.interleave(infiniteCtrl, finite1, finite2),
      "interleaving FINITE DOMAINS using an INFINITE CONTROL DOMAIN should result in a FINITE DOMAIN"
    )

    shouldBeInfinite(
      Gen.interleave(infiniteCtrl, finite1, infinite2),
      "CROSSING DOMAINS using an should result in an INFINITE DOMAIN"
    )

    shouldBeInfinite(
      Gen.interleave(infiniteCtrl, infinite1, infinite2),
      "interleaving INFINITE DOMAINS using an INFINITE CONTROL DOMAIN should result in an INFINITE DOMAIN"
    )
  }

  "interleave" should "interleave two domains together according to control domain values" in {
    import fp.Lazy.Stream

    def ctrl(bs: Boolean*) =
      FiniteDomain(bs.toList)

    val rng = SimpleRNG(123L)

    val infinite1 = InfiniteDomain(Stream.from(1))
    val infinite2 = InfiniteDomain(Stream.from(6))
    val finite1 = infinite1.take(5)
    val finite2 = infinite2.take(5)

    val infiniteCtrlTrue = InfiniteDomain(Stream.constant(true))
    val infiniteCtrlFalse = InfiniteDomain(Stream.constant(false))

    lazy val infiniteStreamCross: Stream[Boolean] = Stream.cons(true, Stream.cons(false, infiniteStreamCross))
    val infiniteCtrlCross = InfiniteDomain(infiniteStreamCross)

    Gen.interleave(ctrl(true, true, true, true), finite1, finite2).finite.toList should equal(Some(List(1, 2, 3, 4)))
    Gen.interleave(ctrl(false, false, false, false), finite1, finite2).finite.toList should equal(Some(List(6, 7, 8, 9)))
    Gen.interleave(ctrl(true, false, true, false), finite1, finite2).finite.toList should equal(Some(List(1, 6, 2, 7)))
    Gen.interleave(ctrl(false, false, false, true), finite1, finite2).finite.toList should equal(Some(List(6, 7, 8, 1)))
    Gen.interleave(infiniteCtrlTrue, infinite1, infinite2).take(5).toList should equal(Some(List(1, 2, 3, 4, 5)))
    Gen.interleave(infiniteCtrlFalse, infinite1, infinite2).take(5).toList should equal(Some(List(6, 7, 8, 9, 10)))
    Gen.interleave(infiniteCtrlCross, infinite1, infinite2).take(6).toList should equal(Some(List(1, 6, 2, 7, 3, 8)))
  }

  "finite domains" should "allow to prove properties" in {
    val booleans = List(true, false)
    Prop.check(forAll(Gen.boolean)(booleans.contains)) should equal(Result.Proven)
    // If the number of test cases is >= than the domain size, the property should be proven.
    Prop.check(forAll(Gen.byte)(_ < 256), maxSize = 300, testCases = 300) should equal(Result.Proven)
  }
}
