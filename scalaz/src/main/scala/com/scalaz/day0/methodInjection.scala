package com.scalaz.day0;

object MethodInjection {
  import Sum.Monoid.Step4.Monoid
  import Sum.Monoid.Step4.Step4_1.m._

  object Bare {
    def plus[A: Monoid](a1: A, a2: A): A = 
      implicitly[Monoid[A]].mappend(a1,a2)

    def show() = ("MethodInjection_Bare", plus(3,4), plus("a","b"))
  }

  // If we want to provide an operator, it's sensible to enrich every possible
  // type that has an instance for Monoid.
  object Scalaz7 {
    trait MonoidOp[A] {
      val F: Monoid[A]
      val value: A
      def |+|(a2: A) = F.mappend(value, a2)
    }

    implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = 
      new MonoidOp[A] {
        val F = implicitly[Monoid[A]]
        val value = a
      }

      // Based on having the monoids defined, we are able to inject the operator
      // to all types with one simple definition.
      def show() = ("MethodInjection_Scalaz7", 3|+|4, "a"|+|"b")
  }
}
