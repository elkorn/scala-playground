package fp.errors

import scala.{ Option => _, Either => _, _ }

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = this match {
    case Left(e) => b
    case Right(a) => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    va <- this
    vb <- b
  } yield f(va, vb)
}

case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A) = {
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    def go(result: Either[E, List[A]], todo: List[Either[E, A]]): Either[E, List[A]] = todo match {
      case Nil => result
      case Left(e) :: t => Left(e)
      case Right(a) :: t => result.flatMap((l) => sequence(t).map((l2) => (a :: l) ++ l2))
    }

    go(Right(Nil), es)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

  def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)((x) => x)
}
