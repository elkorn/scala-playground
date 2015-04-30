package fp.errors

trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case None => default
  }

  def orElse[B >: A](default: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse default

  /*
   Although we can explicitly pattern match on option, we will almost always use the
   higher-order functions.
   */
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
