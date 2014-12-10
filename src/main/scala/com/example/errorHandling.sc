import java.util.regex.{Pattern, PatternSyntaxException}
import scala.annotation.tailrec
object errorHandling {
  val opt = Some(1)
  opt.getOrElse(3)
  None.getOrElse(13)
  opt.flatMap((v) => Some(v + 5))
  opt.map(_ + 5)
  variance(List(-10, -5, 1, -5))
  doesMatch("a.+", "ababa")
  doesMatch("a.+", "bababa")
  doesMatch("a.+[", "bababa")
  map2(Some("a"), Some("b"))(_ ++ _)
  map2(Some("a"), None)(_ ++ _)
  bothMatch_2("a.+", "ab.+", "ababa")
  bothMatch_2("a.+", "bb.+", "ababa")
  bothMatch_2("a.+", "bb.+[", "ababa")
  sequence(List(Some(1), Some(2), Some(3)))
  sequence(List(Some(1), Some(2), Some(3), None))
  traverse(List(1, 2, 3))((a) => Some(a + 5))
  traverse(List(1, 2, 3))((a) =>
    if (a == 3) None
    else Some(a + 10))
  case class Employee(name: String, age: Int, salary: Int)
  for {
    age <- Right(42)
    name <- Left("Invalid name")
    salary <- Right(10000)
  } yield Employee(name, age, salary)
  for {
    age <- Right(42)
    name <- Right("Charles")
    salary <- Right(10000)
  } yield Employee(name, age, salary)
  eitherSequence(List(Right(1), Right(2), Right(3)))
  eitherSequence(List(Right(1), Left("Error"), Right(3)))
  eitherTraverse(List(1, 2, 3))((a) => Right(a + 5))
  eitherTraverse(List(1, 2, 3))((a) =>
    if (a == 3) Left("'a' must not be 3")
    else Right(a + 10))

  // 4.1
  trait Option[+A] {
    def map[B](f: A => B): Option[B]

    def flatMap[B](f: A => Option[B]): Option[B]

    def getOrElse[B >: A](default: => B): B

    def orElse[B >: A](ob: => Option[B]): Option[B]

    def filter(f: A => Boolean): Option[A]
  }

  case class Some[+A](get: A) extends Option[A] {
    def map[B](f: A => B): Option[B] = Some(f(get))

    def flatMap[B](f: A => Option[B]): Option[B] = {
      f(get) match {
        case v: Some[B] => v
        case _ => None
      }
    }

    def getOrElse[B >: A](default: => B): B = get

    def orElse[B >: A](ob: => Option[B]): Option[B] = this

    def filter(f: A => Boolean): Option[A] =
      if (f(get)) this
      else None
  }

  case object None extends Option[Nothing] {
    def map[B](f: Nothing => B): Option[B] = this

    def flatMap[B](f: Nothing => Option[B]): Option[B] = this

    def getOrElse[B >: Nothing](default: => B): B = default

    def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

    def filter(f: Nothing => Boolean): Option[Nothing] = this
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  // 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    for (
      m <- mean(xs);
      v <- mean(xs.map(x => math.pow(x - m, 2)))
    ) yield v
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def matches(pat: Pattern)(s: String): Boolean =
    pat.matcher(s).matches

  def mkMatcher(p: String): Option[String => Boolean] =
    pattern(p) map (matches)

  def mkMatcher_1(p: String): Option[String => Boolean] =
    for {
      p <- pattern(p)
    } yield matches(p)

  def doesMatch(p: String, s: String): Option[Boolean] =
    for {
      m <- mkMatcher_1(p)
    } yield m(s)

  def bothMatch(p1: String, p2: String, s: String): Option[Boolean] =
    for {
      m1 <- mkMatcher(p1);
      m2 <- mkMatcher(p2)
    } yield m1(s) && m2(s)

  def bothMatch_1(p1: String, p2: String, s: String): Option[Boolean] =
    mkMatcher(p1).flatMap(
      m1 => mkMatcher(p2) map (
        m2 => m1(s) && m2(s)))
  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- a;
      b <- b
    } yield f(a, b)
  // 4.4
  def bothMatch_2(p1: String, p2: String, s: String): Option[Boolean] =
    map2(mkMatcher(p1), mkMatcher(p2))((m1, m2) => m1(s) && m2(s))
  // 4.5
  def sequence[A](opts: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def go(opts: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = acc match {
      case None => None
      case Some(acc) => opts match {
        case None :: _ => None
        case Some(v) :: Nil => Some(acc :+ v)
        case Some(v) :: tail => go(tail, Some(acc :+ v))
      }
    }

    go(opts, Some(Nil: List[A]))
  }
  // 4.6
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec
    def go(a: List[A], acc: Option[List[B]]): Option[List[B]] = acc match {
      case None => None
      case Some(acc) => f(a.head) match {
        case None => None
        case Some(b) => if (a.tail.isEmpty) Some(acc :+ b)
        else go(a.tail, Some(acc :+ b))
      }
    }

    go(a, Some(Nil: List[B]))
  }
  // 4.7
  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]

    def flatMap[EE >: E, B >: A](b:A => Either[EE, B]): Either[EE, B]

    def orElse[EE >: E, B>:A](b: =>Either[EE,B])

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
    Either[EE, C]
  }

  case class Left[+E](value: E) extends Either[E, Nothing] {
    def map[B](f: Nothing => B) = this

    def flatMap[EE >: E, B >: Nothing](b: Nothing => Either[EE, B]) = this
    def orElse[EE >: E, B>:Nothing](b: =>Either[EE,B]) = b
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C) = this
  }

  case class Right[+A](value: A) extends Either[Nothing, A] {
    def map[B](f: A => B) = Right(f(value))

    def flatMap[EE >: Nothing, B >: A](b: A => Either[EE, B]) = b(value)
    def orElse[EE >: Nothing, B>:A](b: =>Either[EE,B]) = this
    def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C) = b match {
      case Left(e) => Left(e)
      case Right(b) => Right(f(value,b))
    }
  }
  // 4.8
  def eitherSequence[E,A](eths: List[Either[E,A]]): Either[E,List[A]] = {
    @tailrec
    def go(eths: List[Either[E,A]], acc: Either[E, List[A]]): Either[E, List[A]] = acc match {
      case Left(_) => acc
      case Right(acc) => eths match {
        case Left(err) :: tail => Left(err)
        case Right(v) :: Nil => Right(acc :+ v)
        case Right(v) :: tail => go(tail, Right(acc :+ v))
      }
    }

    go(eths, Right(Nil:List[A]))
  }

  def eitherTraverse[E, A, B](a: List[A])(f: A => Either[E,B]): Either[E, List[B]] = {
    @tailrec
    def go(a: List[A], acc: Either[E,List[B]]): Either[E,List[B]] = acc match {
      case Left(_) => acc
      case Right(acc) => f(a.head) match {
        case Left(err) => Left(err)
        case Right(b) if (a.tail.isEmpty)  => Right(acc :+ b)
        case Right(b) if (!a.tail.isEmpty)  => go(a.tail, Right(acc :+ b))
      }
    }

    go(a, Right(Nil: List[B]))
  }
}

