package com.pureIO.FreeMonad

import javax.swing.text.html.Option

import com.pureIO.FreeMonad

object Console {

  type ConsoleIO[A] = FreeMonad.IO[Console, A]

  trait Console[A]

  case class PrintLn(s: String) extends Console[Unit]

  case object ReadLine extends Console[Option[String]]

}
