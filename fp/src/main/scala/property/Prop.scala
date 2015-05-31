package fp.property

import Prop._
import fp.state.RNG
import fp.state.RNG
import fp.state.SimpleRNG

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
  type MaxSize = Int

  def &&(p1: Prop, p2: Prop): Prop = Prop { (max, n, rng) =>
    p1.check(max, n, rng) match {
      case Passed => p2.check(max, n, rng) match {
        case Passed => Passed
        case f: Falsified => f
      }

      case f: Falsified => f
    }
  }

  def ||(p1: Prop, p2: Prop): Prop = Prop { (max, n, rng) =>
    p1.check(max, n, rng) match {
      case Passed => Passed
      case Falsified(msg, f) => p2.tag(msg).check(max, n, rng)
    }
  }

  def check(
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = SimpleRNG(System.currentTimeMillis)
  ): Result = {
    p.check(maxSize, testCases, rng) match {
      case f @ Falsified(msg, count) =>
        println(s"! Falsified after $count passed tests:\n $msg")
        f
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
        Passed
    }
  }
}

case class Prop(check: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop.&&(this, p)
  def ||(p: Prop): Prop = Prop.||(this, p)
  def tag(msg: String): Prop = {
    val originalCheck = check
    Prop((max, n, rng) => originalCheck(max, n, rng) match {
      case Passed => Passed
      case Falsified(msg2, n) => Falsified(s"$msg\n$msg2", n)
    })
  }
}
