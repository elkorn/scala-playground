package fp.property

import org.scalatest._

class UsageSpec extends FlatSpec with Matchers {
  import Gen._
  import fp.property.domain.FiniteDomain

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

  "interleave" should "interleave two streams together according to control stream values" in {
    import fp.Lazy.Stream

    val s1 = FiniteDomain(Stream.from(1).take(5).toList)
    val s2 = FiniteDomain(Stream.from(6).take(5).toList)
    val bools = FiniteDomain(List(true, false, false, true))
    Gen.interleave(bools, s1, s2).finite.toList should equal(List(1, 6, 2, 7))
  }

  // "cartesian" should "create a cartesian product of nested streams" in {
  //   import fp.Lazy.Stream

  //   val s = Stream(Stream(1, 2), Stream(3, 4), Stream(5, 6))
  //   val expectedResult = Stream(Stream(1, 3, 5), Stream(1, 3, 6), Stream(1, 4, 5), Stream(1, 4, 6), Stream(2, 3, 5), Stream(2, 3, 6), Stream(2, 4, 5), Stream(2, 4, 6))

  //   def deepList[A](s: Stream[Stream[A]]): List[List[A]] =
  //     s.map(_.toList).toList

  //   deepList(Gen.cartesian(s)) should equal(deepList(expectedResult))

  // }

  "finite domains" should "allow to prove properties" in {
    val booleans = List(true, false)
    Prop.check(forAll(Gen.boolean)(booleans.contains)) should equal(Result.Proven)
    // If the number of test cases is >= than the domain size, the property should be proven.
    Prop.check(forAll(Gen.byte)(_ < 256), maxSize = 300, testCases = 300) should equal(Result.Proven)
  }
}
