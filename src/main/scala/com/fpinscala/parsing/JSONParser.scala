package com.fpinscala.parsing

import com.fpinscala.parsing.JSON._

/**
 * Created by elkorn on 1/11/15.
 */
object JSONParser {


  def create[Err, Parser[+ _]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._

    implicit def tokenize(s: String) = token(string(s))

    val spaces = string(" ").many.slice

    def jObject(values: List[(String, JSON)]) = JObject(values.toMap)
    def jArray(values: List[JSON]) = JArray(values.toIndexedSeq)

    def keyValuePair = zip(quotedEscaped, skipLeft(string(":"), value))

    def block[TIn, TOut](start: String, end: String, separator: String)(value: Parser[TIn])(resultMapper: List[TIn] => TOut): Parser[TOut] =
      surround(
        string(start),
        string(end))(
          map(
            separated(
              value,
              string(separator)))(
              resultMapper))

    def obj: Parser[JSON] =
      block("{", "}", ",")(keyValuePair)(jObject)

    def literal: Parser[JSON] =
      orr(
        map(string("null"))(x => JNull),
        map(double)((JNumber(_))),
        as(string("true"))(JBoolean(true)),
        as(string("false"))(JBoolean(false)),
        map(quoted)(JString(_)))

    def array =
      block("[", "]", ",")(value)(values =>
        jArray(values))

    def value: Parser[JSON] = or(array, or(obj, literal))

    def surround[A](begin: Parser[Any], end: Parser[Any])(p: Parser[A]) =
      skipRight(skipLeft(begin, p), end)

    root(skipLeft(spaces, or(obj, array)))
  }
}
