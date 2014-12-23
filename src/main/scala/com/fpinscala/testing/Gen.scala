package com.fpinscala.testing

/**
 * Created by elkorn on 12/23/14.
 */
trait Gen[A] {
  // Not specifying the size of the resulting list allows for greater flexibility.
  // Whatever function that runs the test has the freedom to choose the test size.
  def listOf[A](a: Gen[A]): Gen[List[A]]

  def forAll[A](as: Gen[A])(predicate: A => Boolean): Prop
}


object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Prop {

  import com.fpinscala.testing.Prop.{FailedCase, SuccessCount}

  def &&(other: Prop): Prop = new Prop {
    def check: Boolean = this.check && other.check
  }

  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}