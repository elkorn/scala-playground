import com.fpinscala.BasicExercises

import scala.annotation.tailrec
object fp {
  BasicExercises.fib(3)
  BasicExercises.fib(4)
  BasicExercises.fib(5)
  BasicExercises.fib(6)
  BasicExercises.factorial(5)
  BasicExercises.indexOf[Int](Array(1, 2, 3), {
    case 2 => true
    case _ => false
  })
  BasicExercises.isSorted[Int](Array(1, 2, 3), {
    _ < _
  })
  BasicExercises.isSorted[Int](Array(1, 2, 3), {
    _ > _
  })
  ListHOFs.product(List(2, 2, 3))
  ListHOFs.foldRight(List(1, 2, 3), Nil: List[Int])(_ :: _)
  ListHOFs.length(List(1, 2, 3, 4, 5))
  ListHOFs.reverse(List(1, 2, 3, 4, 5))
  ListHOFs.concat(List(List(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5)))
  ListHOFs.add1(List(1, 2, 3, 4, 5))
  ListHOFs.map(List(1, 2, 3, 4, 5))((a) => a * 12)
  ListHOFs.filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0)
  ListHOFs.flatMap(List(1, 2, 3, 4))((i) => List(i - 1, i, i + 1))
  ListHOFs.filter2(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0)
  ListHOFs.addElements(List(1, 2, 3), List(2,2,2))
  ListHOFs.zipWith(List(1,2,3), List(3,4,5))(_*_)
  ListHOFs.hasSubsequence(List(1,2,3,2,2,5), List(1,3,2))
  ListHOFs.hasSubsequence(List(1,2,3,2,2,5), List(3,2))
  ListHOFs.hasSubsequence(List(1,2,3,2,2,5), List(5))
  ListHOFs.hasSubsequence(List(1,2,3,2,2,5), List(5,2))
  ListHOFs.hasSubsequence(List(1,2,3,2,2,5), List(2,2,5))

  object ListHOFs {
    // 3.10
    def foldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): B = {
      as match {
        case Nil => acc
        case x :: xs => f(x, foldRight(xs, acc)(f))
      }
    }
    @tailrec
    def foldLeft[A, B](as: List[A], acc: B)(f: (A,B)=>B): B = {
      as match {
        case Nil => acc
        case x :: xs => foldLeft(as.tail, f(as.head, acc))(f)
      }
    }
    // 3.11
    def sum(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
    // 3.12
    def product(ns: List[Int]) = foldLeft(ns, 1)(_ * _)
    // 3.13
    def length(ns: List[Int]) = foldLeft(ns, 0)((_, acc) => acc + 1)
    // 3.14
    def reverse(ns: List[Int]) = foldLeft(ns, Nil: List[Int])((y, ys) => ys ::: List(y))
    // 3.15
    def concat(ns: List[List[Int]]) = foldLeft(ns, Nil: List[Int])(_ ++ _)
    // 3.16
    def add1(ns: List[Int]) = foldLeft(ns, Nil: List[Int])((n, result) => (n + 1) :: result)
    // 3.17
    def toString(ns: List[Double]) = foldLeft(ns, Nil: List[String])((n, result) => n.toString :: result)
    // 3.18
    def map[A, B](as: List[A])(f: (A) => B): List[B] = foldLeft(as, Nil: List[B])((n: A, bs: List[B]) => f(n) :: bs)
    // 3.19
    def filter[A](as: List[A])(pred: (A) => Boolean): List[A] = foldLeft(as, Nil: List[A])((a, result) => if (pred(a)) a :: result else result)
    // 3.20
    def flatMap[A, B](as: List[A])(f: (A) => List[B]): List[B] = foldLeft(as, Nil: List[B])((a, bs) => f(a) ::: bs)
    // 3.21
    def filter2[A](as: List[A])(pred: (A) => Boolean): List[A] = flatMap(as) { a => if (pred(a)) List(a) else Nil}
    // 3.22
    def addElements(as: List[Int], bs: List[Int]): List[Int] = {
      @tailrec
      def go(as: List[Int], bs: List[Int], acc: List[Int]): List[Int] = as match {
        case Nil => acc
        case _ => go(as.tail, bs.tail, acc ::: List(as.head + bs.head))
      }
      go(as, bs, Nil)
    }

    // 3.23
    def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
      @tailrec
      def go(as: List[A], bs: List[B], acc: List[C]): List[C] = {
        as match {
          case Nil => acc
          case _ => go(as.tail, bs.tail, acc ::: List(f(as.head, bs.head)))
        }
      }

      go(as, bs, Nil: List[C])
    }

    // 3.24
    @tailrec
    def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
      @tailrec
      def subseq(l: List[A], sub: List[A]): Boolean = {
        if (sub.isEmpty) true
        else if (l.length < sub.length) false
        else if (l.head == sub.head) subseq(l.tail, sub.tail)
        else false
      }

      if (subseq(l, sub)) true
      else if(l.isEmpty) false
      else hasSubsequence(l.tail, sub)
    }
  }

}