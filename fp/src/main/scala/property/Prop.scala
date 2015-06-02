package fp.property

import Prop._
import fp.state.RNG
import fp.state.RNG
import fp.state.SimpleRNG
import scala.util.Left
import scala.util.Right

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int
  type Result = Either[(FailedCase, TestCases), Status]

  sealed trait Status

  object Status {
    case object Unfalsified extends Status
    case object Proven extends Status
    case object Exhausted extends Status
  }

  def &&(p1: Prop, p2: Prop): Prop = Prop { (max, n, rng) =>
    p1.check(max, n, rng) match {
      case Right(Status.Unfalsified) => p2.check(max, n, rng) match {
        case ok @ Right(_) => ok
        case falsified => falsified
      }

      case proven @ Right(Status.Proven) => proven
      case falsified => falsified
    }
  }

  def ||(p1: Prop, p2: Prop): Prop = Prop { (max, n, rng) =>
    p1.check(max, n, rng) match {
      case ok @ Right(_) => ok
      case Left((msg, f)) => p2.tag(msg).check(max, n, rng)
    }
  }

  def check(
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = SimpleRNG(System.currentTimeMillis)
  ): Result = {
    val r = p.check(maxSize, testCases, rng)
    r match {
      case Left((msg, count)) =>
        println(s"! Falsified after $count passed tests:\n $msg")

      case Right(Status.Exhausted) =>
        println(s"+ OK, $testCases tests did not falsify the property.")

      case Right(Status.Unfalsified) =>
        println(s"+ OK, passed after $testCases tests.")

      case Right(Status.Proven) =>
        println(s"+ OK, proved property.")
    }

    r
  }
}

case class Prop(check: (MaxSize, TestCases, RNG) => Prop.Result) {
  def &&(p: Prop): Prop = Prop.&&(this, p)
  def ||(p: Prop): Prop = Prop.||(this, p)
  def tag(msg: String): Prop = {
    val originalCheck = check
    Prop((max, n, rng) => originalCheck(max, n, rng) match {
      case ok @ Right(Status.Unfalsified | Status.Proven) => ok
      case falsified @ Left((fail, count)) => Left((s"$msg\n$fail", count))
    })
  }
}
