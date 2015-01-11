package com.fpinscala.parsing

/**
 * Created by elkorn on 1/10/15.
 */
// Parser[+_] has a type that itself is a type constructor (meta-type?)
trait Parsers[ParseError, Parser[+ _]] {
  self =>

  def orString(s1: String, s2: String): Parser[String] = or(string(s1), string(s2))

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] = ???

  // has to be implicit for promoting String instances to Parser instances.
  implicit def string(s: String): Parser[String] = ???

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = ???

  def run[A](p: Parser[A])(input: String): Either[ParserError, A] = ???

  def countChar(c: Char): Parser[Int] = map(many(char(c)))(_.size)

  def char(c: Char): Parser[Char] = ???

  def many[A](a: Parser[A]): Parser[List[A]] = ???

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = ???

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }

  //  object Laws {
  //    def mapIsStructurePreserving[A](p: Parser[A])(in: Gen[String]): Prop =
  //      equal(p, Parsers.map(p)(a => a))(in)
  //
  //    private def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
  //      Gen.forAll(in)(s => run(p1)(s) == run(p2)(s))
  //  }
}
