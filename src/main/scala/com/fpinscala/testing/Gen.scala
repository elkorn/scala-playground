package com.fpinscala.testing

import com.fpinscala.purestate.{RNG, State}

/**
 * Created by elkorn on 12/23/14.
 */
object Gen {
  // Not specifying the size of the resulting list allows for greater flexibility.
  // Whatever function that runs the test has the freedom to choose the test size.
  //  def listOf[A](a: Gen[A]): Gen[List[A]]

  //  def forAll[A](as: Gen[A])(predicate: A => Boolean): Prop

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(randInt => start + randInt % (stopExclusive - start)))
  }

  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.boolean))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }
}

case class Gen[A](sample: State[RNG, A])


object Prop {
  type FailedCase = String
  type SuccessCount = Int
  // A computation that carries some state along == state action | state transition | statement.
}

trait Prop {

  //  def &&(other: Prop): Prop = new Prop {
  //    def check: Boolean = this.check && other.check
  //  }
  //
  //  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}