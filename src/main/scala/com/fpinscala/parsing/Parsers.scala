package com.fpinscala.parsing

import java.util.regex.Pattern

import com.fpinscala.testing.{Gen, Prop, SGen}

import scala.annotation.tailrec
import scala.util.matching.Regex


/**
 * Created by elkorn on 1/10/15.
 */
// Parser[+_] has a type that itself is a type constructor (meta-type?)
trait Parsers /*[Parser[+ _]]*/ {
  self =>

  /**
   * Defining the Parser type as Location => Result[A] shows that a parser is a kind
   * of a state action that can fail. It receives an input state, and if successful,
   * outputs a value as well as enough information to control how the state should
   * be updated.
   */
  type Parser[+A] = ParseState => Result[A]

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(err) => Failure(f(err))
      case _ => this
    }

    def uncommit: Result[A] = setCommit(false)

    def commit: Result[A] = setCommit(true)

    def setCommit(commit: Boolean): Result[A] = this match {
      case Failure(err, state) => Failure(err, state || commit)
      case _ => this
    }

    /**
     * Increments the number of characters consumed after a successful result.
     */
    def advanceSuccess(consumed: Int): Result[A] = this match {
      case Success(a, offset) => Success(a, offset + consumed)
      case _ => this
    }

    def slice: Result[String]

  }

  case class Slice(length: Int) extends Result[String] {
    def extract(s: String) = Right(s.substring(0, length))

    def slice = this

    override def advanceSuccess(n: Int) = Slice(length + n)
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A] {
    def extract(s: String) = Right(get)

    def slice: Result[String] = Slice(charsConsumed)
  }

  case class Failure(get: ParseError, isCommited: Boolean = false) extends Result[Nothing] {
    def extract(s: String) = Left(get)

    def slice = this
  }

  case class ParseState(loc: Location, isSliced: Boolean = false) {
    def advanceBy(numChars: Int) =
      copy(loc = loc.copy(offset = loc.offset + numChars))

    def input = loc.input.substring(loc.offset)

    def slice(n: Int) = loc.input.substring(loc.offset, loc.offset + n)

    def unslice = setSliced(false)

    def reslice(s: ParseState) = setSliced(s.isSliced)

    private def setSliced(state: Boolean) = copy(isSliced = state)
  }

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }
  }

  case class ParseError(stack: List[(Location, String)]) {
    def push(loc: Location, msg: String): ParseError =
      copy(stack = (loc, msg) :: stack)

    def label[A](msg: String): ParseError =
      ParseError(latestLoc.map((_, msg)).toList)

    def latestLoc: Option[Location] = latest.map(_._1)

    def latest: Option[(Location, String)] =
      stack.lastOption
  }

  // ==============================================================================

  def orString(s1: String, s2: String): Parser[String] = or(string(s1), string(s2))

  def orr[A](ps: Parser[A]*): Parser[A] = {
    @tailrec
    def go(acc: Parser[A], ps: Seq[Parser[A]]): Parser[A] = ps match {
      case Nil => acc
      case p1 :: rest => go(or(acc, p1), rest)
    }

    go(ps.head, ps.tail)
  }

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
    ps => p1(ps) match {
      case Failure(err, false) => p2(ps)
      case other => other
    }

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = ps => p(ps) match {
    case Success(result, consumed) => Success(List.fill(n)(result), consumed)
    case f: Failure => f
  }

  implicit def run[A](p: Parser[A])(input: String): Result[A] = p(ParseState(Location(input)))

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

  private def firstNonMatchingIndex(target: String, source: String, offset: Int = 0): Int = {
    val ns = source.length
    val nt = target.length
    val n = ns min nt

    @tailrec
    def go(current: Int): Int = {
      if (current < n) {
        if (source.charAt(current + offset) != target.charAt(current)) current
        else go(current + 1)
      } else -1
    }

    val result = go(0)

    if (result == -1) {
      if (ns - offset >= nt) -1
      else ns - offset
    }
    else result
  }

  // Does it have to be implicit for promoting String instances to Parser instances?
  // The compiler does not seem to complain about it not being implicit...
  implicit def string(s: String): Parser[String] =
    (ps: ParseState) => {
      firstNonMatchingIndex(s, ps.loc.input, ps.loc.offset) match {
        case -1 => Success(s, s.length)
        case n => Failure(toError(s"'$s'", ps.advanceBy(n)))
      }
    }

  def toError(s: String, input: ParseState): ParseError = {
    ParseError(List((input.loc, s"Expected: $s")))
  }

  /**
   * Try parsing forward as long as `p` does not fail, concatenating the results along the way.
   * If `p` fails, succeed with an empty list.
   */
  implicit def many[A](p: Parser[A]): Parser[List[A]] =
    or(many1(p), succeed(Nil: List[A]))

  /**
   * Recognizes one or more instances of values parsed by `p`.
   * Conceptually this is just `p` followed by `many(p)` (as in string concatenation).
   */
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, nonStrict(many(p)))(_ :: _)

  def skipLeft[B](p0: Parser[Any], pb: => Parser[B]): Parser[B] =
    map2(slice(p0), pb)((_, b) => b)

  def separated[A](p: Parser[A], separators: Parser[Any]): Parser[List[A]] =
    or(separated1(p, separators), succeed(Nil: List[A]))

  def separated1[A](p: Parser[A], separators: Parser[Any]): Parser[List[A]] =
    map2(p, many(skipLeft(separators, p)))(_ :: _)

  /**
   * Book version.
   */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(Nil: List[A])
    else map2(p, listOfN(n - 1, p))(_ :: _)

  /**
   * My basic idea.
   */
  def listOfN1[A](n: Int, p: Parser[A]): Parser[List[A]] =
    or(map(p)((a) => List.fill(n)(a)), succeed(Nil: List[A]))

  /**
   * Optimized variant.
   * The key observation I had while creating this is that the accumulator
   * parser's list is being filled while going down the recursion stack.
   * It prepares all the arguments while going down, and brings the
   * last instance of the accumulator (completely filled) back up the stack.
   *
   * My mind does not cope well with recursion...
   */
  def listOfN2[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    @tailrec
    def work(n: Int, p: Parser[A], acc: Parser[List[A]]): Parser[List[A]] = {
      if (n <= 0) acc
      else work(n - 1, p, map2(p, acc)(_ :: _))
    }

    work(n, p, succeed(Nil: List[A]))
  }

  /**
   * `pb` needs to be non-strict. This is due to `many` being recursive and always evaluating its second
   * argument - which would lead to non-termination.
   */
  def zip[A, B](pa: Parser[A], pb: Parser[B]): Parser[(A, B)] =
    flatMap(pa)(a => map(pb)(b => (a, b)))

  def as[A, B](a: Parser[A])(b: B): Parser[B] =
    map(slice(a))(_ => b)

  implicit def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(a => succeed(f(a)))

  /**
   * This parser always succeeds with the value of `a`, regardless of the input string.
   * string("") will always successfully be parsed - even if the input is empty.
   * @param a some value.
   * @return a
   */
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  implicit def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    ps => p(ps) match {
      case Success(value, consumed) => f(value)(ps.advanceBy(consumed))
        .setCommit(consumed != 0)
        .advanceSuccess(consumed)
      case e: Failure => e
    }

  /**
   * Runs a parser purely to see what portion of the input string it examines.
   * @return The portion of the input string examined by the parser if successful.
   */
  // this impl. cannot be good.
  def slice[A](p: Parser[A]): Parser[String] =
    (ps: ParseState) =>
      p(ps.copy(isSliced = true)).slice


  def root[A](p: Parser[A]): Parser[A] = skipRight(p, eof)

  def skipRight[A](pa: => Parser[A], p0: => Parser[Any]): Parser[A] =
    map2(pa, slice(p0))((a, _) => a)

  /**
   * `pb` needs to be non-strict. This is due to `many` being recursive and always evaluating its second
   * argument - which would lead to non-termination.
   */
  def map2[A, B, C](pa: Parser[A], pb: Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(pa)(a => map(pb)(b => f(a, b)))

  def eof: Parser[String] = regex("\\z".r)

  /**
   * Parse expressions like "0", "1a", "2bb", "3ooo" and so on.
   */
  def contextual(p: Parser[Char]) = flatMap(map(regex("[0-9]+".r))(_.toInt))(listOfN(_, p))

  def whitespace: Parser[String] = regex("\\s*".r)

  def double: Parser[Double] = map(regex("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r))(_.toDouble)

  implicit def regex(r: Regex): Parser[String] =
    (ps: ParseState) =>
      r.findFirstMatchIn(ps.loc.input) match {
        case Some(theMatch) => Success(theMatch.matched, theMatch.start)
        case None => Failure(toError(s"/$r.regex/", ps))
      }

  def token[A](p: Parser[A]): Parser[A] =
    skipRight(p, whitespace)

  // `pair` compiles to String - this is most likely due to how `asStringParser` is defined.
  //  def map2x[A, B, C](pa: Parser[A], pb: Parser[B])(f: (A, B) => C): Parser[C] =
  //    map(pa.zip(pb))(pair => f(pair._1, pair._2))

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def through(s: String): Parser[String] = regex((".*?" + Pattern.quote(s)).r)

  def quoted: Parser[String] = token(skipLeft(string("\""), through("\"").map(_.dropRight(1))))

  def quotedEscaped: Parser[String] = ???

  def label[A](msg: String)(p: Parser[A]): Parser[A] = ps => p(ps).mapError(_.label(msg))

  /**
   * Allows nesting labels.
   */
  def scope[A](name: String)(p: Parser[A]): Parser[A] =
    (ps: ParseState) => (p(ps)).mapError(_.push(ps.loc, name))

  /**
   * Delays committing to a parse.
   */
  def attempt[A](p: Parser[A]): Parser[A] = ps => p(ps).uncommit

  def fail[A](msg: String): Parser[A] = ps => Failure(toError(msg, ps), true)

  private def nonStrict[A](a: => Parser[A]): Parser[A] = a

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def run(s: String): Result[A] = self.run(p)(s)

    def slice: Parser[String] = self.slice(p)

    def zip[B](pb: Parser[B]): Parser[(A, B)] = self.zip(p, pb)
  }

  object Laws {
    type Predicate[A] = A => Boolean

    def mapIsStructurePreserving[A](p: Parser[A])(in: SGen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def equal[A](p1: Parser[A], p2: Parser[A])(in: SGen[String]): Prop =
      Gen.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def succeedAlwaysSucceeds[A](in: SGen[String], out: A): Prop =
      Gen.forAll(in) { s =>
        succeed(out).run(s) == Right(out)
      }

    def zipIsAssociative[A, B, C](pa: Parser[A], pb: Parser[B], pc: Parser[C])(in: SGen[String]): Prop =
      Gen.forAll(in)(isBijection(zip(zip(pa, pb), pc), zip(pa, zip(pb, pc))))

    private def isBijection[A, B, C](pl: Parser[((A, B), C)], pr: Parser[(A, (B, C))]): Predicate[String] =
      (s: String) => pl.map(unbiasLeft).run(s) == pr.map(unbiasRight).run(s)

    private def unbiasLeft[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)

    private def unbiasRight[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)

    def mapAndZipAreMutuallyAssociative[A, B, C](pa: Parser[A], pb: Parser[B])(f: A => B, g: B => C)(in: SGen[String]): Prop =
      Gen.forAll(in) { str =>
        zip(pa.map(f), pb.map(g)).run(str) == zip(pa, pb).map { case (a, b) => (f(a), g(b))}.run(str)
      }

    def labelMustContainErrorMessage[A](p: Parser[A])(in: SGen[String]): Prop =
      Gen.forAll(in.zipWith(Gen.string())) { case (input, msg) =>
        run(label(msg)(p)) match {
          case Left(err: ParseError) => err.latest.get._2 == msg
          case _ => true
        }
      }

    def attemptDoesNotCommitToFailedBranches[A](branchToFail: Parser[A], branchToSucceed: Parser[A])(in: SGen[String]): Prop = {
      equal(or(attempt(flatMap(branchToFail)(_ => fail("test"))), branchToSucceed), branchToSucceed)(in)
    }
  }

}
