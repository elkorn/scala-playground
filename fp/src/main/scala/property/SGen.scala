package fp.property

case class Unsized[A](get: Gen[A]) extends SizedGen[A] {
  def apply(n: Int) = get
}

case class Sized[A](forSize: Int => Gen[A]) extends SizedGen[A] {
  def apply(n: Int) = forSize(n)
}

trait SizedGen[A] {
  def map[B](f: A => B): SizedGen[B] = this match {
    case Unsized(g) => Unsized(g map f)
    case Sized(forSize) => Sized(forSize andThen (_ map f))
  }

  def flatMap[B](f: A => Gen[B]): SizedGen[B] = this match {
    case Unsized(g) => Unsized(g flatMap f)
    case Sized(forSize) => Sized(forSize andThen (_ flatMap f))
  }

  def apply(n: Int): Gen[A]
}
