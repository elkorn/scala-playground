package com.fpinscala.parsing

/**
 * Created by elkorn on 1/10/15.
 */
// Parser[+_] has a type that itself is a type constructor (meta-type?)
trait Parsers[ParseError, Parser[+ _]] {
  def char(c: Char): Parser[Char] = ???

  def orString(s1: String, s2: String): Parser[String] = or(string(s1), string(s2))

  def string(s: String): Parser[String] = ???

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = ???

  def run[A](p: Parser[A])(input: String): Either[ParserError, A] = ???
}
