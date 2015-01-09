package com.fpinscala.testing

import com.fpinscala.purestate.RNG
import com.fpinscala.testing.Prop.{FailedCase, MaxSize, TestCases}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  // How many test cases to run
  type TestCases = Int
  type MaxSize = Int

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.simple(System.currentTimeMillis())): Result = {
    lazy val result = p.run(maxSize, testCases, rng)
    result match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

    result
  }

  def check(p: => Boolean): Prop = Prop {
    (_, _, _) => if (p) Proved else Falsified("()", 0)
  }
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(other: Prop) = Prop(
    (max, n, rng) => {
      run(max, n, rng) match {
        case Passed => other.run(max, n, rng)
        case someFailure => someFailure
      }
    }
  )

  def ||(other: Prop) = Prop(
    (max, n, rng) => {
      run(max, n, rng) match {
        case Falsified(msg, _) => other.tag(msg).run(max, n, rng)
        case ok => ok
      }
    }
  )

  def tag(msg: FailedCase) = Prop(
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(error, completed) => Falsified(s"$msg\n$error", completed)
      case ok => ok
    }
  )


}