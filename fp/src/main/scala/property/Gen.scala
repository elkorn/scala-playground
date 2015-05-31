package fp.property

import fp.state.{ RNG, State }
import scala.annotation.tailrec
import fp.Lazy.Stream

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

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack strace:\n${e.getStackTrace().mkString("\n")}"

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(a)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

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

  def flatMap[A, B](g: Gen[A])(f: A => Gen[B]): Gen[B] = Gen(State(rng => {
    val (a, s1) = g.sample.run(rng)
    val (b, s2) = f(a).sample.run(s1)
    (b, s2)
  }))

  def map[A, B](g: Gen[A])(f: A => B): Gen[B] = Gen(State(rng => {
    val (a, s) = g.sample.run(rng)
    (f(a), s)
  }))
}

case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen.flatMap(this)(f)
  def listOfN(n: Int): Gen[List[A]] =
    Gen.listOfN(n, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(this.listOfN)

  def union(other: Gen[A]): Gen[A] =
    Gen.union(this, other)

  def unsized: SGen[A] = SGen(_ => this)

  def map[B](f: A => B) = Gen.map(this)(f)

  val apply = sample.run
}
