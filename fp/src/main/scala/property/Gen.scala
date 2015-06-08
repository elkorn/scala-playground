package fp.property

import fp.state.SimpleRNG
import fp.state.{ RNG, State }
import scala.annotation.tailrec
import fp.Lazy.{ Stream, Cons, Empty }
import scala.util.Left
import scala.util.Right
import domain.{ Domain, FiniteDomain, InfiniteDomain, EmptyDomain }

object Gen {
  object Result {
    val Proven: Prop.Result = Right(Prop.Status.Proven)
    val Unfalsified: Prop.Result = Right(Prop.Status.Unfalsified)
    val Exhausted: Prop.Result = Right(Prop.Status.Exhausted)
    def Falsified(failedCase: Prop.FailedCase, successCount: Prop.SuccessCount): Prop.Result =
      Left((failedCase, successCount))
  }

  def listOf[A](gen: Gen[A]): SizedGen[List[A]] = Sized(listOfN(_, gen))

  def nonEmptyListOf[A](gen: Gen[A]): SizedGen[List[A]] = Sized(n => listOfN(if (n == 0) 1 else n, gen))

  // TODO: Work with InfiniteDomain[FiniteDomain[A]] and FiniteDomain[FiniteDomain[A]] only - sized lists are to be generated here - so the length must be finite.
  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] = {
    val stream = gen.domain.foldRight(Stream[A]())(Stream.cons(_, _))
    val streamDomain = Domain.cartesian(FiniteDomain(stream.map(_ => stream).take(n).toList))
    Gen(State.sequence(List.fill(n)(gen.sample)), streamDomain.map(_.toList))
  }

  def unit[A](a: A): Gen[A] = Gen(State.unit(a), FiniteDomain(List(a)))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean), FiniteDomain(List(true, false)))

  def byte: Gen[Byte] = Gen(State(RNG.int).map(x => (x % 256).toByte), FiniteDomain(Stream.from(0).take(255).map(_.toByte).toList))

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack strace:\n${e.getStackTrace().mkString("\n")}"

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) =>
      {
        def go(cur: Int, end: Int, domain: Domain[A], onDomainExhausted: Prop.Result): Prop.Result = {
          def check(h: A, t: Domain[A]): Prop.Result =
            try {
              if (f(h)) go(cur + 1, end, t, onDomainExhausted)
              else Result.Falsified(h.toString(), cur)
            } catch {
              case e: Exception => Result.Falsified(buildMsg(h, e), cur)
            }

          if (domain.isExhausted) onDomainExhausted
          else {
            domain match {
              case x @ FiniteDomain(hf, t) =>
                // Take the cur and end values only if the number of test cases is smaller than the domain size.
                if (n < x.size && cur == end) Result.Unfalsified
                else check(hf(), t)
              case x @ InfiniteDomain(hf, t) => check(hf(), t)
            }
          }
        }

        // TODO this can be done more cleanly since we have domain size support.
        go(0, n / 3, a.domain, Result.Proven) match {
          case Result.Unfalsified =>
            val rands = InfiniteDomain(randomStream(a)(rng))
            go(n / 3, n, rands, Result.Unfalsified)
          case provenOrExhausted => provenOrExhausted
        }
      }
  }

  def forAll[A](g: SizedGen[A])(f: A => Boolean): Prop =
    forAll(g.apply(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      {
        val casesPerSize = n / max + 1
        val props: Stream[Prop] = Stream.from(0).take(max + 1).map(i => forAll(g(i))(f))
        val finalProp = props.map(p => Prop((max, _, rng) => p.check(max, casesPerSize, rng))).toList.reduce(_ && _)
        finalProp.check(max, n, rng).right.map {
          case Prop.Status.Proven => Prop.Status.Exhausted
          case x => x
        }
      }
  }

  // TODO This domain does not seem correct.
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(
      State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)),
      FiniteDomain((start to (stopExclusive - start)).toList)
    )

  def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    zip(choose(start, stopExclusive), choose(start, stopExclusive))((_, _))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  // def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] =
  //   s1.append(s2)
  // s1.zipAll(s2) flatMap {
  //   case (a1, a2) => Stream((a1.toList ++ a2.toList): _*)
  // }

  def uniform: Gen[Double] = Gen(State(RNG.double), InfiniteDomain(Stream.from(0).map(_.toDouble)))
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    def distribution: Domain[Boolean] = InfiniteDomain(randomStream(uniform.map(_ < g1Threshold))(SimpleRNG(12345L)))

    Gen(
      State(RNG.double).flatMap(x => if (x < g1Threshold) g1._1.sample else g2._1.sample),
      interleave(distribution, g1._1.domain, g2._1.domain)
    )
  }

  def interleave[A](ctrl: Domain[Boolean], s1: Domain[A], s2: Domain[A]): Domain[A] = ctrl match {
    case EmptyDomain =>
      EmptyDomain
    case FiniteDomain(hf, ctrlTail) => hf() match {
      case true => s1 match {
        case x @ Domain(h, t) if !x.isExhausted =>
          val tail = interleave(ctrlTail, t, s2)
          FiniteDomain(Stream(h()).append(tail.toStream).toList)
        case EmptyDomain => s2
      }
      case false => s2 match {
        case x @ Domain(h, t) if !x.isExhausted =>
          val tail = interleave(ctrlTail, s1, t)
          FiniteDomain(Stream(h()).append(tail.toStream).toList)
        case EmptyDomain => s1
      }
    }

    case InfiniteDomain(hf, ctrlTail) => hf() match {
      // The tails have to be lazy here to allow infinite domains in both s1 and s2.
      case true => s1 match {
        case EmptyDomain => s2
        case x @ FiniteDomain(h, t) if !x.isExhausted =>
          lazy val tail = interleave(ctrlTail, t, s2)
          if (s2.isFinite) FiniteDomain(Stream(h()).append(tail.toStream).toList)
          else InfiniteDomain(Stream(h()).append(tail.toStream))
        case x @ InfiniteDomain(h, t) if !x.isExhausted =>
          lazy val tail = interleave(ctrlTail, t, s2)
          InfiniteDomain(Stream(h()).append(tail.toStream))
      }

      case false => s2 match {
        case EmptyDomain => s1
        case x @ FiniteDomain(h, t) if !x.isExhausted =>
          lazy val tail = interleave(ctrlTail, s1, t)
          if (s1.isFinite) FiniteDomain(Stream(h()).append(tail.toStream).toList)
          else InfiniteDomain(Stream(h()).append(tail.toStream))
        case x @ InfiniteDomain(h, t) if !x.isExhausted =>
          lazy val tail = interleave(ctrlTail, s1, t)
          InfiniteDomain(Stream(h()).append(tail.toStream))
      }
    }
  }

  def zip[A, B, C](ga: Gen[A], gb: Gen[B])(f: (A, B) => C): Gen[C] = for {
    a <- ga
    b <- gb
  } yield f(a, b)

  def flatMap[A, B](g: Gen[A])(f: A => Gen[B]): Gen[B] =
    Gen(
      g.sample.flatMap(a => f(a).sample),
      g.domain match {
        case x @ FiniteDomain(_, _) => x.flatMap(x => f(x).domain.finite)
        case x @ InfiniteDomain(_, _) => x.flatMap(x => f(x).domain.infinite)
      }
    )

  def map[A, B](g: Gen[A])(f: A => B): Gen[B] = Gen(g.sample.map(f), g.domain.map(f))

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
    g map (i => (_ => i))

  // Don't know how to deal with this...
  def genHof[A, B, C](ga: Gen[A])(f: A => B => C): Gen[B => C] =
    ga map (f)

  def map2[A, B, C](ga: Gen[A], gb: Gen[B])(f: (A, B) => C): Gen[C] = for {
    a <- ga
    b <- gb
  } yield f(a, b)

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }
}

case class Gen[A](sample: State[RNG, A], domain: Domain[A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen.flatMap(this)(f)
  def listOfN(n: Int): Gen[List[A]] =
    Gen.listOfN(n, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(this.listOfN)

  def union(other: Gen[A]): Gen[A] =
    Gen.union(this, other)

  def unsized: SizedGen[A] = Unsized(this)

  def map[B](f: A => B) = Gen.map(this)(f)
  def map2[B, C](g: Gen[B])(f: (A, B) => C) = Gen.map2(this, g)(f)

  def listOf(): SizedGen[List[A]] = Gen.listOf(this)
  def nonEmptyListOf(): SizedGen[List[A]] = Gen.nonEmptyListOf(this)

  def **[B](gb: Gen[B]): Gen[(A, B)] =
    (this map2 gb)((_, _))

  val apply = sample.run
}
