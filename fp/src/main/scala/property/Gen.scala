package fp.property

import fp.state.{ RNG, State }
import scala.annotation.tailrec

object Gen {
  def listOf[A](gen: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] = Gen(State(rng => {
    @tailrec
    def go(left: Int, rng: RNG, result: List[A]): (List[A], RNG) = {
      if (left > 0) {
        val (a, s) = gen.sample.run(rng)
        go(left - 1, s, a :: result)
      } else (result, rng)
    }

    go(n, rng, Nil: List[A])
  }))

  def unit[A](a: A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(rng => {
    val (n, s) = rng.nextInt
    (n < 0, s)
  }))

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(rng => {
      val (n, s) = rng.nextInt
      (n % stopExclusive + start, s)
    }))

  def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    Gen(State(rng => {
      val (a1, s2) = choose(start, stopExclusive).sample.run(rng)
      val (a2, s3) = choose(start, stopExclusive).sample.run(s2)
      ((a1, a2), s3)
    }))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap(x => if (x < g1Threshold) g1._1.sample else g2._1.sample))
  }

}

case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(State(rng => {
    val (a, s1) = this.sample.run(rng)
    val (b, s2) = f(a).sample.run(s1)
    (b, s2)
  }))

  def listOfN(n: Int): Gen[List[A]] =
    Gen.listOfN(n, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(this.listOfN)

  def union(other: Gen[A]): Gen[A] =
    Gen.union(this, other)

}
