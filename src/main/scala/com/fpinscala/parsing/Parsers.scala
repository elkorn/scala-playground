package com.fpinscala.parsing

import com.fpinscala.testing.{Gen, Prop}

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

  implicit def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  def countChar(c: Char): Parser[Int] = map(many(char(c)))(_.size)

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  implicit def many[A](a: Parser[A]): Parser[List[A]] = ???

  implicit def map[A, B](a: Parser[A])(f: A => B): Parser[B] = ???

  /**
   * This parser always succeeds with the value of `a`, regardless of the input string.
   * string("") will always successfully be parsed - even if the input is empty.
   * @param a some value.
   * @return a
   */
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def run(s: String): Either[ParseError, A] = self.run(p)(s)
  }

  object Laws {
    def mapIsStructurePreserving[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    private def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Gen.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def succeedAlwaysSucceeds[A](in: Gen[String], out: A): Prop =
      Gen.forAll(in) { s =>
        succeed(out).run(s) == Right(out)
      }
  }

}
