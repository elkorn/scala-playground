package fp.parsing

trait Parsers[ParseError, Parser[+_]] { self =>
  /*
   Infix operator support.
  */
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def string(s: String): Parser[String]

  def char(c: Char): Parser[Char] = string(c.toString()) map (_.charAt(0))

  // This is the "unit" parser (or the equivalent of "fork" in Par)
  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  // This parser can be run to see what portion of a string it examines.
  def slice[A](p: Parser[A]): Parser[String]

  def or[A](a1: Parser[A], a2: Parser[A]): Parser[A]

  def repetitions[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, p many)(_ :: _) or succeed(Nil: List[A])

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(Nil: List[A])
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def count[A](p: Parser[A]): Parser[Int] =
    p.many.slice.map(_.size) // References String.size, which runs in constant time as it does not need to construct anything.

  def atLeastOne[A](p: Parser[A]): Parser[List[A]] =
    p.many.map {
      case Nil => throw new RuntimeException("Errors need to be handled better :)")
      case nonEmptyList => nonEmptyList
    }

  def zeroOrManyAndAtLeastOne[A](p1: Parser[A], p2: Parser[A]): Parser[(Int, Int)] =
    p1.count ** p2.atLeastOne.count

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def map2[A, B, C](pa: Parser[A], pb: Parser[B])(f: (A, B) => C): Parser[C] = for {
    a <- pa
    b <- pb
  } yield f(a, b)

  def product[A, B](pa: Parser[A], pb: Parser[B]): Parser[(A, B)] =
    pa.map2(pb)((_, _))

  case class ParserOps[A](p: Parser[A]) {
    def |[AA >: A](p2: Parser[AA]): Parser[AA] = self.or(p, p2)
    def or[AA >: A](p2: Parser[AA]): Parser[AA] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def map2[B, C](pb: Parser[B])(f: (A, B) => C) = self.map2(p, pb)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def many = self.many(p)
    def slice = self.slice(p)
    def product[B](pb: Parser[B]) = self.product(p, pb)
    def **[B](pb: Parser[B]) = self.product(p, pb)
    def count = self.count(p)
    def atLeastOne = self.atLeastOne(p)
  }

  object Laws {
    import fp.property._

    def equal[A](p1: Parser[A], p2: Parser[A])(input: Gen[String]): Prop = {
      Gen.forAll(input) { s =>
        run(p1)(s) == run(p2)(s)
      }
    }

    def succeedLaw[A](a: A)(input: Gen[String]): Prop =
      Gen.forAll(input) {
        s => run(succeed(a))(s) == Right(a)
      }

    def mapLaw[A](p: Parser[A])(input: Gen[String]): Prop =
      equal(p, p.map(a => a))(input)

    // This is not a correct law for slice.
    // def sliceLaw[A](p: Parser[A])(input: Gen[String]): Prop =
    //   Gen.forAll(input) { s =>
    //     run(slice(p))(s) == Right(s)
    //   }

    object Product {
      private def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)
      private def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)

      def isAssociative[A, B, C](pa: Parser[A], pb: Parser[B], pc: Parser[C])(input: Gen[String]): Prop =
        equal(
          (pa ** pb) ** pc map unbiasL,
          pa ** (pb ** pc) map unbiasR
        )(input)

      def isAssociativeWithMap[A, B, C](pa: Parser[A], pb: Parser[B])(f: A => C, g: B => C)(input: Gen[String]): Prop =
        equal(
          pa.map(f) ** pb.map(g),
          (pa ** pb) map { case (a, b) => (f(a), g(b)) }
        )(input)
    }
  }
}
