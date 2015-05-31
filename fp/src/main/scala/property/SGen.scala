package fp.property

object SGen {
  def flatMap[A, B](a: SGen[A])(f: A => Gen[B]): SGen[B] = SGen(a.forSize andThen (_ flatMap f))

  def map[A, B](a: SGen[A])(f: A => B): SGen[B] = SGen(a.forSize andThen (_ map f))

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = ???
}

case class SGen[A](forSize: Int => Gen[A]) {
  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen.flatMap(this)(f)
  def map[B](f: A => B): SGen[B] = SGen.map(this)(f)
  val apply = forSize
}
