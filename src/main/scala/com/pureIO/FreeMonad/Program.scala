package com.pureIO.FreeMonad

import com.pureIO.FreeMonad.Console.{Console, PrintLn, ReadLine}

trait Run[F[_]] {
  def apply[A](expr: F[A]): (A, Run[F])
}

object RunConsole extends Run[Console] {
  def apply[A](c: Console[A]) = c match {
    case ReadLine => (Option(scala.io.StdIn.readLine()), RunConsole
  }

  )
  case PrintLn(s) => (println(s), RunConsole)
}

}

abstract class App[F[_]](R: Run[F]) {

  // Core idea which can be extrapolated to any domain.
  // The `run` function executes an instruction queue
  // through recursion.
  def run[A](io: IO[F, A]): A =
    io match {
      case Pure(a) => a
      case Request(req: F[A], k) =>
        val (e, r2) = R(req)
        run(r2)(k(e))
    }

  def pureMain(args: Array[String]): IO[F, Unit]

  def main(args: Array[String]): IO[F, Unit] =
    run(pureMain(args))
}