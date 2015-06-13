package fp.parsing

import MyParser._
import scala.collection.mutable.Stack
import scala.util.matching.Regex

object MyParser {
  type Parser[+A] = ParseState => Result[A]

  implicit def resultToEither[A](result: Result[A]): Either[ParseError, A] = result match {
    case ParseSuccess(v, _) => Right(v)
    case ParseFailure(err, _) => Left(err)
  }

  // implicit def locationToLocationWithLabel(location: Location) = LocationWithLabel(location)

  sealed trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case ParseFailure(err, isCommited) => ParseFailure(f(err), isCommited)
      case success => success
    }

    def uncommit: Result[A] = this match {
      case ParseFailure(e, true) => ParseFailure(e, false)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case ParseSuccess(a, m) => ParseSuccess(a, n + m)
      case _ => this
    }

    def commit(isCommited: Boolean): Result[A] = this match {
      case ParseFailure(e, previouslyCommited) => ParseFailure(e, previouslyCommited || isCommited)
      case _ => this
    }
  }

  case class ParseSuccess[A](get: A, charsConsumed: Int) extends Result[A]

  case class ParseFailure(get: ParseError, isCommited: Boolean) extends Result[Nothing]

  case class ParseState(location: Location) {
    def advanceBy(chars: Int) =
      copy(location = location.copy(offset = location.offset + chars))

    def input = location.input.substring(location.offset)

    def slice(n: Int) = location.input.slice(location.offset, location.offset + n)

    val toError = location.toError _
  }
}

trait MyParsers extends Parsers[Parser] {
  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = p(ParseState(Location(input)))

  def string(s: String): Parser[String] = {
    case state @ ParseState(_) if state.input.startsWith(s) => ParseSuccess(s, s.length)
    case state => ParseFailure(state.toError(s"""Expected: "$s", got "${state.input.substring(0, s.length min state.input.length)}"."""), false)
  }

  def fail[A](msg: String = "fail() called."): Parser[A] =
    state => ParseFailure(
      state.toError(msg),
      true
    )

  def regex(r: Regex): Parser[String] = state => {
    val msg = s"regex $r"
    r.findPrefixOf(state.input) match {
      case Some(str) => ParseSuccess(str, str.length())
      case None => ParseFailure(state.toError(msg), false)
    }
  }

  def slice[A](p: Parser[A]): Parser[String] = state => p(state) match {
    case ParseSuccess(_, count) => ParseSuccess(state.slice(count), count)
    case ParseFailure(err, isCommited) => ParseFailure(err, isCommited)
  }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    state => p(state).mapError(_.push(state.location, msg))

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    state => p(state).mapError(_.label(msg))

  def attempt[A](p: Parser[A]): Parser[A] = p(_).uncommit

  def or[A](a1: Parser[A], a2: Parser[A]): Parser[A] = state => a1(state) match {
    case ParseFailure(e, false) => a2(state)
    case other => other
  }

  def succeed[A](a: A): Parser[A] = _ => ParseSuccess(a, 0)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = state => p(state) match {
    case ParseSuccess(a, n) =>
      f(a)(state.advanceBy(n)).commit(n != 0).advanceSuccess(n)
    case fail @ ParseFailure(_, _) => fail
  }
}
