package fp.property

import fp.state.SimpleRNG
import fp.state.{ RNG, State }
import scala.annotation.tailrec
import fp.Lazy.{ Stream, Cons, Empty }
import scala.util.Left
import scala.util.Right

trait Domain[A] {
  def isFinite: Boolean
  def isExhausted: Boolean
}

object Domain {
  // Stream is sealed - have to deal with that somehow.
  // A Domain should expose the same methods as a stream and delegate most of its methods to the underlying stream.
  // Calling the head getter of the stream should return a new Domain of a reduced size.
  // Also, calling methods from the `take*` and `drop*` family should modify the counter accordingly.

  case object EmptyDomain extends Domain[Nothing] {
    def isFinite = true
    def isExhausted = true
  }

  case class FiniteDomain[A](size: Int, s: Stream[A]) extends Domain[A] {
    def isFinite = true
    def isExhausted = size == 0
  }

  case class InfiniteDomain[A](s: Stream[A]) extends Domain[A] {
    def isFinite = false
    def isExhausted = false
  }
}

object Gen {
  import Prop.Result
  type Domain[A] = Stream[Option[A]]
  val infiniteDomain: Domain[Nothing] = Stream(None)
  def finiteDomain[A](a: Stream[A]): Domain[A] = a map (Some(_))
  def finiteDomain[A](as: A*): Domain[A] = Stream(as: _*) map (Option(_))

  def listOf[A](gen: Gen[A]): SGen[List[A]] = Sized(listOfN(_, gen))

  def nonEmptyListOf[A](gen: Gen[A]): SGen[List[A]] = Sized(n => listOfN(if (n == 0) 1 else n, gen))

  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(gen.sample)), cartesian(Stream.constant(gen.domain).take(n)).map(l => fp.errors.Errors.sequence(l.toList)))

  def map2Stream[A, B, C](sa: Stream[A], sb: Stream[B])(f: (A, B) => C): Stream[C] = for {
    a <- sa
    b <- sb
  } yield f(a, b)

  def cartesian[A](s: Stream[Stream[A]]): Stream[Stream[A]] = {
    s.foldRight(Stream(Stream[A]()))((h, t) => map2Stream(h, t)(Stream.cons(_, _)))
  }

  def unit[A](a: A): Gen[A] = Gen(State.unit(a), finiteDomain(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean), finiteDomain(true, false))

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack strace:\n${e.getStackTrace().mkString("\n")}"

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      {
        import fp.property.Prop.Status

        def go(cur: Int, end: Int, domain: Stream[Option[A]], onEnd: Int => Result): Result = {
          if (cur == end) Right(Status.Unfalsified)
          else domain match {
            case Cons(hf, t) => hf() match {
              case Some(h) => try {
                // TODO: This is pretty arbitrary. Create a proper Domain abstraction that wraps a Stream and holds the information abount
                if (f(h)) go(cur + 1, end, t(), onEnd)
                else Left((h.toString(), cur))
              } catch {
                case e: Exception => Left((buildMsg(h, e), cur))
              }
              case None => onEnd(cur)
            }
            case _ => Right(Status.Unfalsified)
          }
        }

        go(0, n / 3, a.domain, _ => Right(Status.Proven)) match {
          case Right((Status.Unfalsified)) =>
            val rands = randomStream(a)(rng).map(Some(_))
            go(n / 3, n, rands, _ => Right(Status.Unfalsified))
          case provenOrExhausted => provenOrExhausted
        }

      }
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.apply(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      {
        val casesPerSize = (n + (max - 1)) / max
        val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
        val finalProp = props.map(p => Prop { (max, _, rng) => p.check(max, casesPerSize, rng) }).toList.reduce(_ && _)
        finalProp.check(max, n, rng)
      }
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(
      State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)),
      finiteDomain(Stream.from(start).take(stopExclusive - start))
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

  def uniform: Gen[Double] = Gen(State(RNG.double), infiniteDomain)
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    def distribution: Stream[Boolean] = randomStream(uniform.map(_ < g1Threshold))(SimpleRNG(12345L))

    Gen(State(RNG.double).flatMap(x => if (x < g1Threshold) g1._1.sample else g2._1.sample), interleave(distribution, g1._1.domain, g2._1.domain))
  }

  def interleave[A](ctrl: Stream[Boolean], s1: Stream[A], s2: Stream[A]): Stream[A] = ctrl.headOption map {
    case true => s1 match {
      case Cons(h, t) => Stream.cons(h(), interleave(ctrl drop 1, t(), s2))
      case _ => s2
    }
    case false => s2 match {
      case Cons(h, t) => Stream.cons(h(), interleave(ctrl drop 1, t(), s1))
      case _ => s1
    }
  } getOrElse Stream.empty

  def zip[A, B, C](ga: Gen[A], gb: Gen[B])(f: (A, B) => C): Gen[C] = for {
    a <- ga
    b <- gb
  } yield f(a, b)

  def flatMap[A, B](g: Gen[A])(f: A => Gen[B]): Gen[B] = Gen(g.sample.flatMap(a => f(a).sample), g.domain.flatMap {
    case None => infiniteDomain
    case Some(a) => f(a).domain
  })

  def map[A, B](g: Gen[A])(f: A => B): Gen[B] = Gen(g.sample.map(f), g.domain.map(_.map(f)))
}

case class Gen[A](sample: State[RNG, A], domain: Gen.Domain[A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen.flatMap(this)(f)
  def listOfN(n: Int): Gen[List[A]] =
    Gen.listOfN(n, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(this.listOfN)

  def union(other: Gen[A]): Gen[A] =
    Gen.union(this, other)

  def unsized: SGen[A] = Unsized(this)

  def map[B](f: A => B) = Gen.map(this)(f)

  def listOf(): SGen[List[A]] = Gen.listOf(this)
  def nonEmptyListOf(): SGen[List[A]] = Gen.nonEmptyListOf(this)

  val apply = sample.run
}
