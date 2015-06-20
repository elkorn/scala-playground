package fp.property

import Gen._
import org.scalatest.{ FlatSpec, Matchers }
import Prop.Status._

class PropSpec extends FlatSpec with Matchers {
  val gen = fp.state.SimpleRNG(123L)
  val args = (1, gen)

  "&&" should "create a conjunction of two Props" in {
    val passing = Prop((_, _, _, _) => Right(Unfalsified))
    val failing1 = Prop((_, n, _, _) => Left(("foo", n)))
    val failing2 = Prop((_, n, _, _) => Left(("bar", n + 1)))

    (passing && passing).check(1, 1, false, gen) should equal(Right(Unfalsified))
    (passing && failing1).check(1, 1, false, gen) should equal(Left(("foo", 1)))
    (failing1 && passing).check(1, 1, false, gen) should equal(Left(("foo", 1)))
    (failing2 && failing1).check(1, 1, false, gen) should equal(Left(("bar", 2)))
  }

  "||" should "create an alternative of two Props" in {
    val passing = Prop((_, _, _, _) => Right(Unfalsified))
    val failing1 = Prop((_, n, _, _) => Left(("foo", n)))
    val failing2 = Prop((_, n, _, _) => Left(("bar", n + 1)))

    (passing || passing).check(1, 1, false, gen) should equal(Right(Unfalsified))
    (passing || failing1).check(1, 1, false, gen) should equal(Right(Unfalsified))
    (failing1 || passing).check(1, 1, false, gen) should equal(Right(Unfalsified))
    (failing2 || failing1).check(1, 1, false, gen) should equal(Left(("bar\nfoo", 1)))
  }
}
