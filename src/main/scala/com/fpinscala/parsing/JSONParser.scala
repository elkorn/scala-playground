package com.fpinscala.parsing

/**
 * Created by elkorn on 1/11/15.
 */
object JSONParser {


  def create[Err, Parser[+ _]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._

    val spaces = char(' ').many.slice

    def obj: Parser[JSON] = ???
    def literal: Parser[JSON] = ???

    def array =
      surround(
        char('['),
        char(']'))(
          map(
            separated(
              value,
              char(',')))(
              values =>
                JSON.JArray(values.toIndexedSeq)))

    def value: Parser[JSON] = or(array, or(obj, literal))

    def surround[A](begin: Parser[Any], end: Parser[Any])(p: Parser[A]) =
      skipRight(skipLeft(begin, p), end)

  }
}
