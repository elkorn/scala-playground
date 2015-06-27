package fp.commonStructures

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A,B)]): (F[A], F[B])
  def codistribute[A, B](fab: (F[A], F[B])): F[(A,B)]

  def codistributeEither[A, B](fab: Either[F[A], F[B]]): F[Either[A,B]]
}

object Functor {
  // List is a functor. The Functor[List] instance constitutes the proof that it is
  def list = new Functor[List] {
    def map[A,B](fa: List[A])(f: A => B): List[B] =
      fa map f

    def distribute[A,B](fab: List[(A,B)]): (List[A], List[B]) =
      (fab map (_._1), fab map (_._2))

    def codistributeEither[A, B](fab: Either[List[A], List[B]]): List[Either[A,B]] = fab match {
      case Left(list) => list map (Left(_))
      case Right(list) => list map (Right(_))
    }

    def codistribute[A, B](fab: (List[A], List[B])): List[(A,B)] =
      fab._1.zip(fab._2)
  }
}
