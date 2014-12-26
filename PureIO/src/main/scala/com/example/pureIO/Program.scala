package com.pureIO

sealed trait RealWorld

abstract class Program {
  private val realWorld = new RealWorld {}

  // End of the world. Call all the effects.
  final def main(args: Array[String]): Unit =
    pureMain(args).apply


  def pureMain(args: IndexedSeq[String]): IO[Unit] = {

  }
}