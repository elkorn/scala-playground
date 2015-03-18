package com.scalaz.day0;

object FoldLeftTypeclass {
  object FoldLeftList {
    import Sum.Monoid.Step4.Monoid
    import Sum.Monoid.Step4.Step4_1.m._

    def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B) = xs.foldLeft(b)(f)

    def sum[A: Monoid](xs: List[A]): A = {
      val m = implicitly[Monoid[A]]
      FoldLeftList.foldLeft(xs, m.mzero, m.mappend)
    }

    def show() = ("FoldLeftList",sum(1::2::Nil),sum("a"::"b"::Nil))
  }

  object Generic {
    // We want to generalize the List out of the FoldLeft abstraction.
    trait FoldLeft[F[_]] {
      def foldLeft[A, B](xs: F[A], b: B, f: (B, A) => B): B
    }

    object FoldLeft {
      // Watch out for name collisions! E.g. with `List`.
      implicit val FLList: FoldLeft[List] = new FoldLeft[List] {
        def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B): B = 
          xs.foldLeft(b)(f)
      }
    }

    import Sum.Monoid.Step4.Monoid
    import Sum.Monoid.Step4.Step4_1.m._
    import FoldLeft._

    def sum[M[_]: FoldLeft, A: Monoid](xs: M[A]): A = {
      val m = implicitly[Monoid[A]]
      val fl = implicitly[FoldLeft[M]]
      fl.foldLeft(xs, m.mzero, m.mappend)
    }

    // We now have abstracted out both the container being folded over as well
    // as the folding operation.
    def show() = ("FoldLeft", sum(List(1,2,3,4)), sum(List("a", "b", "c")))

    // The traits Monoid and FoldLeft correspond to Haskell's typeclasses.
  }
}
