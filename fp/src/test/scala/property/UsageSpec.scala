package fp.property

import org.scalatest._

class UsageSpec extends FlatSpec with Matchers {
  import Gen._

  val domain = choose(-10, 10)
  "max" should "return the maximum value from a list" in {
    val maxProp = forAll(nonEmptyListOf(domain)) { ns =>
      {
        val max = ns.max
        !ns.exists(_ > max)
      }
    }

    Prop.check(maxProp) should equal(Passed)
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

    Prop.check(maxProp) should equal(Passed)

  }

}
