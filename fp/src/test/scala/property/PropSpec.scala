package fp.property

import Gen._
import org.scalatest.{ FlatSpec, Matchers }

class PropSpec extends FlatSpec with Matchers {
  val gen = fp.state.SimpleRNG(123L)
  val args = (1, gen)

  "&&" should "create a conjunction of two Props" in {
    val passing = Prop((_, _, _) => Passed)
    val failing1 = Prop((_, n, _) => Falsified("foo", n))
    val failing2 = Prop((_, n, _) => Falsified("bar", n + 1))

    (passing && passing).check(1, 1, gen) should equal(Passed)
    (passing && failing1).check(1, 1, gen) should equal(Falsified("foo", 1))
    (failing1 && passing).check(1, 1, gen) should equal(Falsified("foo", 1))
    (failing2 && failing1).check(1, 1, gen) should equal(Falsified("bar", 2))
  }

  "||" should "create an alternative of two Props" in {
    val passing = Prop((_, _, _) => Passed)
    val failing1 = Prop((_, n, _) => Falsified("foo", n))
    val failing2 = Prop((_, n, _) => Falsified("bar", n + 1))

    (passing || passing).check(1, 1, gen) should equal(Passed)
    (passing || failing1).check(1, 1, gen) should equal(Passed)
    (failing1 || passing).check(1, 1, gen) should equal(Passed)
    (failing2 || failing1).check(1, 1, gen) should equal(Falsified("bar\nfoo", 1))
  }
}
