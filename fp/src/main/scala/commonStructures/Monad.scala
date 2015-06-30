package fp.commonStructures

import fp.parsing.Parsers
import fp.property.Gen
import fp.Lazy.Stream
import fp.state.State
import fp.state.parallelism.Par
import fp.state.parallelism.parallelism.Par


trait Monad[F[_]] {
  def unit[A](a: A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma) (a => map(mb)(b => f(a,b)))
}

object Monad {
  val gen = new Monad[Gen] {
    def unit[A](a: A): Gen[A] = Gen.unit(a)
    def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val par = new Monad[Par] {
    def unit[A](a: A):Par[A] =Par.unit(a)
    def flatMap[A,B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  def parser[P[+_]](p: Parsers[P]) = new Monad[P] {
    def unit[A](a: => A): P[A] = p.succeed(a)
    def flatMap[A,B](ma: P[A])(f: A => P[B]): P[B] =
      p.flatMap(ma)(f)
  }

  def option = new Monad[Option] {
    def unit[A](a: A): Option[A] = Option(a)
    def flatMap[A,B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  def list = new Monad[List] {
    def unit[A](a: A): List[A] = List(a)
    def flatMap[A,B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  def stream = new Monad[Stream] {
    def unit[A](a: A) = Stream(a)
    def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f
  }

  class StateMonad[S] {
    type StateS[A] = State[S, A]

    val get = new Monad[StateS] {
      def unit[A](a: A) = State.unit(a)
      def flatMap[A,B](ma: StateS[A])(f: A => StateS[B]): StateS[B] =
        ma flatMap f
    }
  }
}
