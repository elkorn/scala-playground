package com.fpinscala.testing

import com.fpinscala.purestate.{RNG, State}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(x => f(x).sample))

  def listOfN(n: Int): Gen[List[A]] = {
    Gen.listOfN(n, this)
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(size => listOfN(size))
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  // A computation that carries some state along == state action | state transition | statement.
}

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

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(result => if (result) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val chooseG1 = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(v => if (v <= chooseG1) g1._1.sample else g2._1.sample))
  }
}


trait Prop {

  //  def &&(other: Prop): Prop = new Prop {
  //    def check: Boolean = this.check && other.check
  //  }
  //
  //  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}