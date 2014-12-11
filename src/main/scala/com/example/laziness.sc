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
  stream.flatMap((v) => Stream(v + 1)).toList
  Stream(1, 2, 3, 4).map(_ + 10).filter((v) => {
    println("filter")
    v % 2 == 0
  }).toList
  Stream.cons(11, Stream(2, 3, 4).map(_ + 10)).filter(_ % 2 == 0)
  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]

    def isEmpty: Boolean = uncons.isEmpty

    def head: Option[A]

    def tail: Stream[A]

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

    def append[B >: A](other: Stream[B]): Stream[B] =
      foldRight(other)((head, tail) => Stream.cons(head, tail))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Stream.empty: Stream[B])((a, b) => f(a) append b)
  }
  object Stream {
    def empty[A]: Stream[A] =
      new Stream[A] {
        def uncons = None

        def head = None

        def tail = Stream.empty

        def toList = Nil
      }

    // Note the non-strict arguments.
    def cons[A](theHead: => A, theTail: => Stream[A]): Stream[A] =
      new Stream[A] {
        lazy val uncons = Some((theHead, theTail))

        def head = Some(theHead)

        def tail = theTail

        def toList = {
          @tailrec
          def go(tail: => Stream[A], acc: List[A]): List[A] = {
            tail.uncons match {
              case None => acc
              case Some((head: A, tail: Stream[A])) => go(tail, acc :+ head)
            }
          }

          go(theTail, List(theHead))
        }
      }

    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
    }
  }
  lazy val ones: Stream[Int] = Stream.cons(1, ones)

  // 5.7
  def consts[A](value: A): Stream[A] = Stream.cons(value, consts(value))
  consts(5).take(5).toList

  // 5.8
  def from(value: Int): Stream[Int] = Stream.cons(value, from(value + 1))
  from(3).take(8).toList

  // 5.9
  def fibs(): Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] =
      Stream.cons(n1, go(n2, n1 + n2))
    go(0, 1)
  }
  fibs().take(6).toList
}