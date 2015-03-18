package com.scalaz.day0;

object Sum {
  object Fold {
    def sum(xs: List[Int]) = xs.foldLeft(0) {_ + _} 
  }

  object Monoid {
    object Step1 {
      object IntMonoid {
        def mappend(a: Int, b: Int): Int = a + b
        def mzero: Int = 0
      }

      def sum(xs: List[Int]) = xs.foldLeft(IntMonoid.mzero)(IntMonoid.mappend)  
    }
    
    object Step2 {
      trait Monoid [A] {
        def mappend(a: A, b: A): A
        def mzero: A
      }

      object IntMonoid extends Monoid[Int] {
        def mappend(a: Int, b: Int): Int = a + b
        def mzero: Int = 0
      }

      def sum(xs: List[Int], m: Monoid[Int]) = xs.foldLeft(m.mzero)(m.mappend)  
    }

    object Step3 {
      trait Monoid [A] {
        def mappend(a: A, b: A): A
        def mzero: A
      }

      object IntMonoid extends Monoid[Int] {
        def mappend(a: Int, b: Int): Int = a + b
        def mzero: Int = 0
      }

      def sum[A](xs: List[A], m: Monoid[A]) = xs.foldLeft(m.mzero)(m.mappend)  

      def show() = sum(1::2::Nil, IntMonoid)
    }

    object Step4 {
      import annotation.implicitNotFound
      @implicitNotFound("No member of type class Monoid in scope for ${T}")
      trait Monoid [A] {
        def mappend(a: A, b: A): A
        def mzero: A
      }

      object Step4_1 {
        object m {
          implicit object Ints extends Monoid[Int] {
            def mappend(a: Int, b: Int): Int = a + b
            def mzero: Int = 0
          }

          implicit object Strings extends Monoid[String] {
            def mappend(a: String, b: String): String = a + b
            def mzero: String = ""
          }
        }

        import m._

        def sum[A:Monoid](xs: List[A]) = {
          val m = implicitly[Monoid[A]]
          xs.foldLeft(m.mzero)(m.mappend)  
        }

        def show() = sum(1::2::Nil)
      }

      object Step4_2 {
        private object m {
          implicit object Sum extends Monoid[Int] {
            def mappend(a: Int, b: Int): Int = a + b
            def mzero: Int = 0
          }
          
          implicit object Mul extends Monoid[Int] {
            def mappend(a: Int, b: Int): Int = a * b
            def mzero: Int = 1
          }
        }

        import m._

        // Here, the Mul monoid would take precedence - definition order matters
        // when dealing with implicits.
        // def sum[A:Monoid](xs: List[A]) = {
        //   val m = implicitly[Monoid[A]]
        //   xs.foldLeft(m.mzero)(m.mappend)  
        // }

        // Thanks to monoids, we now have a function that generalizes on the 
        // folding operation.
        def sum[A](xs: List[A])(implicit m: Monoid[A]) = xs.foldLeft(m.mzero)(m.mappend)

        def showDefault() = sum(1::2::Nil)
        def showSum() = sum(1::2::Nil)(m.Sum)
      }
    }
  }
}
