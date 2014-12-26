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

  def unsized: SGen[A] = SGen(_ => this)
}


object Gen {
  // Not specifying the size of the resulting list allows for greater flexibility.
  // Whatever function that runs the test has the freedom to choose the test size.
  //  def listOf[A](a: Gen[A]): Gen[List[A]]

  private def buildMsg[A](value: A, exception: Exception): Prop.FailedCase =
    s"test case: $value\n" +
      s"generated an exception: ${exception.getMessage}\n" +
      s"stack trace:\n ${exception.getStackTrace.mkString("\n")}"

  def forAll[A](as: Gen[A])(predicate: A => Boolean): Prop = Prop({
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (predicate(a)) Passed
        else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  })

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


  private def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((head, state)) => Stream.cons(head, unfold(state)(f))
    case None => Stream.empty
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = unfold(rng)(rng => Some(g.sample.run(rng)))
}