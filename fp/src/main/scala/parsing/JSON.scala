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

    def notQuote = "[^\"\']*".r

    def str: Parser[String] = (notQuote surround (char('\''), char('\''))) | (notQuote surround (char('"'), char('"')))
    def sep(c: Char) = whitespace ** char(c) ** whitespace
    def jNull = string("null").map(_ => JNull)
    def jStr: Parser[JString] = str.map(JString(_))
    def jNumber: Parser[JNumber] = token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)
      .map(str => JNumber(str.toDouble)).label("number literal")
    def jBool = (string("true") or string("false")).map(str => JBool(str.toBoolean))
    def jLiteral: Parser[JSON] = jNull | jStr | jNumber | jBool
    def jArray: Parser[JArray] = jLiteral
      .manySeparated(sep(','))
      .surround(sep('['), sep(']'))
      .map(list => JArray(list.toIndexedSeq))

    def kv: Parser[(String, JSON)] = for {
      _ <- whitespace
      k <- str
      _ <- sep(':')
      v <- jLiteral | jArray | jObj
      _ <- whitespace
    } yield (k, v)

    def jObj: Parser[JObject] = kv.manySeparated(sep(',')).surround(sep('{'), sep('}'))
      .map(list => JObject(list.toMap))
  }

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    val ops = JSONParserLiteralOps(P)
    import ops._

    (for {
      _ <- whitespace
      json <- jObj
      _ <- whitespace
      _ <- eof
    } yield json)
  }
}
