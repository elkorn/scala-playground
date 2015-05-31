package fp.property

import Prop._
import fp.state.RNG

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int

  def &&(p1: Prop, p2: Prop): Prop = Prop { (n, rng) =>
    p1.check(n, rng) match {
      case Passed => p2.check(n, rng) match {
        case Passed => Passed
        case f: Falsified => f
      }

      case f: Falsified => f
    }
  }

  def ||(p1: Prop, p2: Prop): Prop = Prop { (n, rng) =>
    p1.check(n, rng) match {
      case Passed => Passed
      case Falsified(msg, f) => p2.tag(msg).check(n, rng)
    }
  }
}

case class Prop(check: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop.&&(this, p)
  def ||(p: Prop): Prop = Prop.||(this, p)
  def tag(msg: String): Prop = {
    val originalCheck = check
    Prop((n, rng) => originalCheck(n, rng) match {
      case Passed => Passed
      case Falsified(msg2, n) => Falsified(s"$msg\n$msg2", n)
    })
  }
}
