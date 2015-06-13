package fp.parsing

trait JSON

object JSON {

  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  case class JSONParserLiteralOps[Parser[+_]](P: Parsers[Parser]) {
    import P._

    def whitespace = "\\s".r.many.slice
    def notQuote = "[^\"\']*".r

    def str: Parser[String] = (notQuote surround (char('\''), char('\''))) | (notQuote surround (char('"'), char('"')))
    def sep(c: Char) = whitespace ** char(c) ** whitespace
    def jNull = string("null").map(_ => JNull)
    def jStr: Parser[JString] = str.map(JString(_))
    def jNumber: Parser[JNumber] = "\\d+(\\.\\d+)?".r.map(str => JNumber(str.toDouble))
    def jBool = "true|false".r.map(str => JBool(str.toBoolean))
    def literal: Parser[JSON] = jNull | jStr | jNumber | jBool
    def jArray: Parser[JArray] = literal
      .manySeparated(sep(','))
      .surround(sep('['), sep(']'))
      .map(list => JArray(list.toIndexedSeq))
  }

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    val ops = JSONParserLiteralOps(P)
    import ops._

    def kv: Parser[(String, JSON)] = for {
      k <- str
      _ <- sep(':')
      v <- literal | jArray | obj
    } yield (k, v)

    def obj: Parser[JObject] = kv.manySeparated(sep(',')).surround(sep('{'), sep('}'))
      .map(list => JObject(list.toMap))

    for {
      _ <- whitespace
      json <- literal | jArray | obj
      _ <- whitespace
    } yield json
  }
}
