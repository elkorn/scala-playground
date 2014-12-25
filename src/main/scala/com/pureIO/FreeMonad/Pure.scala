package com.pureIO.FreeMonad

// F is a generic functor. It can be e.g. a List.
sealed trait IO[F[_], A]

// No interactinos with the outside world.
case class Pure[F[_], A](a: A) extends IO[F, A]

// A request for the external world to do something.
case class Request[F[_], I, A](
                                req: F[I],
                                k: I => IO[F[I], A]) extends IO[F[I], A]
