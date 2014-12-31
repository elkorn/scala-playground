package com.fpinscala.testing

import com.fpinscala.purestate.RNG
import com.fpinscala.testing.Prop.{MaxSize, FailedCase, TestCases}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  // How many test cases to run
  type TestCases = Int
  type MaxSize = Int
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