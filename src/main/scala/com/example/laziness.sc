import scala.annotation.tailrec

object laziness {
  def pair(i: => Int) = {
    lazy val j = i // delay evaluation until first referenced and cache the result
    (j, j)
  }

  pair {
    println("hi")
    1 + 41
  }
  val stream: Stream[Int] = Stream(1, 2, 3, 4, 5, 6)
  stream.toList
  stream.take(0).toList
  stream.take(2).toList
  stream.take(12).toList
  stream.takeWhile(_ <= 4).toList
  stream.any(_ == 6)
  stream.any(_ > 6)
  stream.every(_ < 7)
  stream.every(_ < 6)
  stream.takeWhile2(_ < 3).toList
  stream.map(_ + 5).toList
  stream.filter(_ % 2 == 1).toList
  stream.flatMap((v)=>Stream(v+1)).toList

  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]

    def isEmpty: Boolean = uncons.isEmpty

    // 5.1
    def toList: List[A]

    // 5.2
    def take(n: Int): Stream[A] = {
      if (!this.isEmpty && n > 0) Stream.cons(uncons.get._1, uncons.get._2.take(n - 1))
      else Stream.empty
    }

    // 5.3
    def takeWhile(p: A => Boolean): Stream[A] = {
      if (!this.isEmpty && p(uncons.get._1)) Stream.cons(uncons.get._1, uncons.get._2.takeWhile(p))
      else Stream.empty
    }

    // The combining function `f` is non-strict in its second parameter.
    def foldRight[B](z: => B)(f: (A, => B) => B): B = uncons match {
      case Some((head, tail)) => f(head, tail.foldRight(z)(f))
      case None => z
    }

    def any(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    // 5.4
    def every(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    // 5.5
    def takeWhile2(p: A => Boolean): Stream[A] =
      foldRight(Stream.empty: Stream[A])(
        (a, b) =>
          if (p(a)) Stream.cons(a, b)
          else Stream.empty
      )

    // 5.6
    def map[B](f: A => B): Stream[B] =
      foldRight(Stream.empty: Stream[B])((a, b) =>
        Stream.cons(f(a), b))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(Stream.empty: Stream[A])(
        (a, b) =>
          if (p(a)) Stream.cons(a, b)
          else b
      )

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Stream.empty: Stream[B])((a, b) => {
        Stream.cons(f(a).uncons.get._1, b)
      })
  }

  object Stream {
    def empty[A]: Stream[A] =
      new Stream[A] {
        def uncons = None

        def toList = Nil
      }

    // Note the non-strict arguments.
    def cons[A](head: => A, tail: => Stream[A]): Stream[A] =
      new Stream[A] {
        lazy val uncons = Some((head, tail))

        def toList = {
          @tailrec
          def go(tail: => Stream[A], acc: List[A]): List[A] = {
            tail.uncons match {
              case None => acc
              case Some((head: A, tail: Stream[A])) => go(tail, acc :+ head)
            }
          }

          go(tail, List(head))
        }
      }

    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
    }
  }

}