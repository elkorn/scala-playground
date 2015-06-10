package fp.parsing

trait JSON

object JSON {

  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._

    val space = char(' ')
    val newline = char('\n')
    val tab = char('\t')
    val whitespace = (space | newline | tab).many.slice
    val notQuote ="[^\"\']*".r

    val jNull = string("null").map(_ => JNull)
    val str: Parser[String] = (notQuote surround(char('\''), char('\''))) | (notQuote surround(char('"'), char('"')))
    val jStr: Parser[JString] = str.map(str => JString(str))
    val number: Parser[JNumber] = "\\d+(\\.\\d+)?".r.map(str => JNumber(str.toDouble))
    val bool = "true|false".r.map(str => JBool(str.toBoolean))
    val literal: Parser[JSON] = jNull | jStr | number | bool
    def sep(c: Char) = whitespace ** char(',') ** whitespace
    val array: Parser[JArray] = literal
      .manySeparated(sep(','))
      .surround(sep('['), sep(']'))
      .map(list => JArray(list.toIndexedSeq))

    lazy val kv: Parser[(String, JSON)] = for {
        k <- str
        _ <- sep(':')
        v <- literal | array | obj
      } yield (k, v)

    lazy val obj:Parser[JObject]  = kv.manySeparated(sep(',')).surround(sep('{'), sep('}'))
      .map(list => JObject(list.toMap))

    for {
      _ <- whitespace
      json <- literal | array | obj
      _ <- whitespace
    } yield json
  }
}
