package fp.hof

import scala.annotation.tailrec

object Hof {
  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n == 0) acc
      else go(n - 1, acc * n)

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, next: Int): Int = n match {
      case 0 => prev
      case 1 => next
      case x => go(n - 1, next, prev + next)
    }

    go(n, 0, 1)
  }

  def isSorted[A](as: List[A], ordered: (A, A) => Boolean): Boolean = as match {
    case Nil => true
    case a1 :: a2 :: Nil => ordered(a1, a2)
    case a1 :: more => if (ordered(a1, more.head)) isSorted(more, ordered)
    else false
  }
}
