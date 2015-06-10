package fp.property

case class Unsized[A](get: Gen[A]) extends SizedGen[A] {
  def apply(n: Int) = get
  def **[B](sgb: SizedGen[B]) = sgb match {
    case Unsized(getB) => Unsized(get ** getB)
    case Sized(_) => Sized(n => this(n) ** sgb(n))
  }
}

case class Sized[A](forSize: Int => Gen[A]) extends SizedGen[A] {
  def apply(n: Int) = forSize(n)
  def **[B](sgb: SizedGen[B]) = Sized(n => this(n) ** sgb(n))
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
  def **[B](sgb: SizedGen[B]): SizedGen[(A, B)]
}
