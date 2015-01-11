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

  // Does it have to be implicit for promoting String instances to Parser instances?
  // The compiler does not seem to complain about it not being implicit...
  implicit def string(s: String): Parser[String] = ???

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = ???

  implicit def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  def suboptimalCountChar(c: Char): Parser[Int] = map(many(char(c)))(_.size)

  /**
   * The difference between this and suboptimalCountChar is that `_.size`
   * counts the length of a string here (constant time) instead of a list (linear time).
   *
   * An important observation here is that even if the `p.many.map(_.size)` parser generates
   * an intermediate list when run, this will not.
   * This hints that `slice` is *primitive* - it will have to have access to the internal
   * parser state.
   * @return The count of character `c` occurrences in given input.
   */
  def countChar(c: Char): Parser[Int] = char(c).many.slice.map(_.size)

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  /**
   * Runs a parser purely to see what portion of the input string it examines.
   * @return The portion of the input string examined by the parser if successful.
   */
  def slice[A](p: Parser[A]): Parser[String] = ???

  /**
   * Try parsing forward as long as `p` does not fail, concatenating the results along the way.
   * If `p` fails, succeed with an empty list.
   */
  implicit def many[A](p: Parser[A]): Parser[List[A]] =
    or(map2(p, many(p))(_ :: _), succeed(Nil: List[A]))

  /**
   * Recognizes one or more instances of values parsed by `p`.
   * Conceptually this is just `p` followed by `many(p)` (as in string concatenation).
   */
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def zip[A, B](pa: Parser[A], pb: Parser[B]): Parser[(A, B)] = ???

  implicit def map[A, B](a: Parser[A])(f: A => B): Parser[B] = ???

  def map2[A, B, C](pa: Parser[A], pb: Parser[B])(f: (A, B) => C): Parser[C] =
    map(zip(pa, pb))(pair => f(pair._1, pair._2))

  // `pair` compiles to String - this is most likely due to how `asStringParser` is defined.
  //  def map2x[A, B, C](pa: Parser[A], pb: Parser[B])(f: (A, B) => C): Parser[C] =
  //    map(pa.zip(pb))(pair => f(pair._1, pair._2))

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

    def slice: Parser[String] = self.slice(p)

    def zip[B](pb: Parser[B]): Parser[(A, B)] = self.zip(p, pb)
  }

  object Laws {
    type Predicate[A] = A => Boolean

    def mapIsStructurePreserving[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    private def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Gen.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def succeedAlwaysSucceeds[A](in: Gen[String], out: A): Prop =
      Gen.forAll(in) { s =>
        succeed(out).run(s) == Right(out)
      }

    def zipIsAssociative[A, B, C](pa: Parser[A], pb: Parser[B], pc: Parser[C])(in: Gen[String]): Prop =
      Gen.forAll(in)(isBijection(zip(zip(pa, pb), pc), zip(pa, zip(pb, pc))))

    private def isBijection[A, B, C](pl: Parser[((A, B), C)], pr: Parser[(A, (B, C))]): Predicate[String] =
      (s: String) => pl.map(unbiasLeft).run(s) == pr.map(unbiasRight).run(s)

    private def unbiasLeft[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)

    private def unbiasRight[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)

    def mapAndZipAreMutuallyAssociative[A, B, C](pa: Parser[A], pb: Parser[B])(f: A => B, g: B => C)(in: Gen[String]): Prop =
      Gen.forAll(in) { str =>
        zip(pa.map(f), pb.map(g)).run(str) == zip(pa, pb).map { case (a, b) => (f(a), g(b))}.run(str)
      }
  }

}
