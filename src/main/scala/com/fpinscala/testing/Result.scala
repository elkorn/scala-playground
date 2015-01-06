package com.fpinscala.testing

import com.fpinscala.testing.Prop.{FailedCase, SuccessCount}


// This is equivalent to the previous  type Result = Option[(FailedCase, SuccessCount)]
// but it's more explicit about what's going on - we have a Passed obj
// instead of None
sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified = true
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}