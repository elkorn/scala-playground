package com.scalaz.day0;

object Polymorphism {
  object Parametric {
    def head[A](xs: List[A]): A = xs(0)
    case class Car(name: String)

    def show() = head(Car("Cadillac") :: Car("Honda") :: Nil)
  }

  object Subtype {
    // Not flexible: the trait has to be mixed in during the datatype definition.
    // Ergo, this will not work for Ints or Strings OOTB.
    trait Plus[A] {
      def plus(a2: A): A
    }

    def plus[A <: Plus[A]](a1: A, a2: A): A = a1.plus(a2)
  }

  object AdHoc {
    // 1. We can provide separate function definitions for different types of A,
    // 2. We can provide function definitions for types without source code access (e.g. for Ints).
    // 3. Function definitions can be enabled or disable in different scopes.
    trait Plus[A] {
      def plus(a1: A, a2: A): A
    }

    object Plus {
      implicit object Int extends Plus[Int] {
        def plus(a1: Int, a2: Int) = a1 + a2
      }

      implicit object Str extends Plus[String] {
        def plus(a1: String, a2: String) = a1 + a2
      }
    }

    def plus[A: Plus](a1: A, a2: A): A = implicitly[Plus[A]].plus(a1, a2)


    def show() = plus(12,13)
    def showString() = plus("a", "b")
  }
}
