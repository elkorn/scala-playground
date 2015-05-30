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

}

case class Gen[A](sample: State[RNG, A])
