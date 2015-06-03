package fp.parsing

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]
  def string(s: String): Parser[String]
  def or[A](a1: Parser[A], a2: Parser[A]): Parser[A]
  def repetitions[A](n: Int, p: Parser[A]): Parser[List[A]]
  def many[A](p: Parser[A]): Parser[List[A]]

  def count[A](p: Parser[A]): Parser[Int] =
    map(many(p))(_.size)

  def atLeastOne[A](p: Parser[A]): Parser[List[A]] =
    map(many(p)) {
      case Nil => throw new RuntimeException("Errors need to be handled better :)")
      case nonEmptyList => nonEmptyList
    }

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  /*
   Infix operator support.
  */
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[AA >: A](p2: Parser[AA]): Parser[AA] = self.or(p, p2)
    def or[AA >: A](p2: Parser[AA]): Parser[AA] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many = self.many(p)
  }
}
