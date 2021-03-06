package com.fp.testing

import com.fp.adt.Tree
import com.fp.adt.Tree.{Branch, Leaf}
import com.fp.purestate.{RNG, State}

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

  def listOf: SGen[List[A]] = Gen.listOf(this)

  def zipWith[B](other: Gen[B]): Gen[(A, B)] = Gen(State(s => {
    val (r1, rng1) = sample.run(s)
    val (r2, rng2) = other.sample.run(rng1)
    ((r1, r2), rng2)
  }))
}

trait Cogen[-A] {
  def sample(a: A, rng: RNG): RNG
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
    (max, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (predicate(a)) Passed
        else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  })

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      val casesPerSize = (n + (max - 1)) / max
      println(max, n, rng, casesPerSize, (n.min(max)) + 1);
      val props: Stream[Prop] =
        Stream.from(0).take((n.min(max)) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop {
          (max, _, rng) => p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
    }
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

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

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(n, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOf(n.max(1)))

  def char(): Gen[Char] =
    Gen.choose(0, 127).map(_.toChar)

  def charN(n: Int): Gen[List[Char]] =
    listOfN(n, char())

  def stringN(n: Int): Gen[String] =
    charN(n).map(_.mkString)

  def string(): SGen[String] =
    listOf(char()).map(_.mkString)

  object TreeDecision extends Enumeration {
    val None, Left, Right, Both = Value
  }

  private def genDecision(): Gen[TreeDecision.Value] = Gen.choose(0, 4).map(TreeDecision.apply)

  def genTree[A](values: Gen[A], maxDepth: Int = 100): Gen[Tree[A]] = Gen(State(recursiveTree(values, 0, maxDepth)))

  private def recursiveTree[A](values: Gen[A], depth: Int, maxDepth: Int)(rng: RNG): (Tree[A], RNG) = {
    val (value, rng2) = values.sample.run(rng)

    if (depth == maxDepth - 1) return (Leaf(value), rng2)

    val (decision, rng3) = genDecision().sample.run(rng2)
    decision match {
      case TreeDecision.None => (Leaf(value), rng3)
      case TreeDecision.Left => {
        val (r1, rng4) = recursiveTree(values, depth + 1, maxDepth)(rng3)
        (Branch(r1, Leaf(value)), rng4)
      }
      case TreeDecision.Right => {
        val (r1, rng4) = recursiveTree(values, depth + 1, maxDepth)(rng3)
        (Branch(Leaf(value), r1), rng4)
      }
      case TreeDecision.Both => {
        val (left, rng4) = recursiveTree(values, depth + 1, maxDepth)(rng3)
        val (right, rng5) = recursiveTree(values, depth + 1, maxDepth)(rng4)
        (Branch(left, right), rng5)
      }
    }
  }

  private def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((head, state)) => Stream.cons(head, unfold(state)(f))
    case None => Stream.empty
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = unfold(rng)(rng => Some(g.sample.run(rng)))

  // This function creates a generator of functions that take a string and use it to affect the generation of
  // random values.
  def genStringFn[A](g: Gen[A]): Gen[String => A] = Gen {
    State(rng => {
      val (res, rng2) = rng.nextInt
      val f = (s: String) => g.sample.run(RNG.simple(res.toLong ^ s.hashCode.toLong))._1
      (f, rng2)
    })
  }

  // This function uses a co-generator to steer the output generator.
  def genCogen[A, B](in: Cogen[A])(out: Gen[B]): Gen[A => B] = Gen {
    State(rng => {
      val (res, rng2) = rng.nextInt
      val f = (a: A) => out.sample.run(in.sample(a, RNG.simple(res)))._1
      (f, rng2)
    })
  }
}