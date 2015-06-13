package fp.parsing

import MyParser._
import scala.collection.mutable.Stack

object MyParser {
  type Parser[+A] = LocationWithLabel => Result[A]

  implicit def resultToEither[A](result: Result[A]): Either[ParseError, A] = result match {
    case ParseSuccess(v) => Right(v)
    case ParseFailure(err) => Left(err)
  }

  implicit def locationToLocationWithLabel(location: Location) = LocationWithLabel(location)

  sealed trait Result[+A]

  case class ParseSuccess[A](get: A) extends Result[A]

  object ParseFailure {
    def direct(location: LocationWithLabel) =
      ParseFailure(ParseError(Stack((location.location, location.label))))
  }

  case class LocationWithLabel(location: Location, label: String = "") {
    def tail = location.tail
  }

  case class ParseFailure(error: ParseError) extends Result[Nothing]
}

trait MyParsers extends Parsers[Parser] {
  def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input, 0))

  def string(s: String): Parser[String] = location => location.tail match {
    case input if input == s => ParseSuccess(input)
    case input => ParseFailure.direct(location.copy(label = s"'$input' is not a string I want."))
  }

  def fail[A]() =
    location => ParseFailure.direct(location.copy(label = s"Failing because you told me to."))

  def slice[A](p: Parser[A]): Parser[String] = location => p.map(_ => location.tail)(location)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = p(_) match {
    // Here it should go forward.
    case ParseSuccess(a) => f(a)
    case fail => fail
  }

  def or[A](a1: Parser[A], a2: Parser[A]): Parser[A] = location => a1(location) match {
    case success @ ParseSuccess(_) => success
    case ParseFailure(err) => a2(location)
  }

  def attempt[A](p: Parser[A]): Parser[A] = p(_)

  def label[A](msg: String)(p: Parser[A]): Parser[A] = location => p(location.copy(label = msg))

  // I don't really want to think about this now.
  def scope[A](msg: String)(p: Parser[A]): Parser[A] = label(s"scope:$msg")(p)
}
