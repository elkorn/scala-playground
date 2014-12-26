package com.fpinscala.testing

import com.fpinscala.purestate.RNG
import com.fpinscala.testing.Prop.{FailedCase, TestCases}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  // How many test cases to run
  type TestCases = Int
}

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(other: Prop) = Prop(
    (n, rng) => {
      run(n, rng) match {
        case Passed => other.run(n, rng)
        case someFailure => someFailure
      }
    }
  )

  def ||(other: Prop) = Prop(
    (n, rng) => {
      run(n, rng) match {
        case Falsified(msg, _) => other.run(n, rng)
        case ok => ok
      }
    }
  )

  def tag(msg: FailedCase) = Prop(
    (n, rng) => run(n, rng) match {
      case Falsified(error, completed) => Falsified(s"$msg\n$error", completed)
      case ok => ok
    }
  )
}