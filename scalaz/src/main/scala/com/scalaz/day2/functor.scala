package com.scalaz.day2;

object FunctorTypeclass {
  import scalaz._;
  import Scalaz._;

  /*
    The Functor typeclass is basically for things that can be mapped over.
  */

 /*
    Why does the tuple have only the last element mapped? Runar answers this:
    One use case for `map` on tuples is so-called "dependency injection".
    E.g. there are cases where you'll have a computation `A => B` with a context
    of some type `C`. So your overall type is `(C, A) => B`.
    You may want to compute something based on the context, while still 
    preserving the context, so you would do:
    def compute(p: (C, A)): (C, B) =
      p.cojoin.map(someFun)
    The someFun function receives both the `C` and the `A` to compute a `B`. 
    This kind of thing is useful when you're working in e.g. the State monad or 
    the Reader comonad.
*/

  // By this we lift the given partial function into the realm of lists.
  val f = ( Functor[List].lift {(_: Int) * 3} )

 def show() =
   ("FunctorTypeclass",
     (1,2,3) map {_+1},
     // Having functions as Functors allows composition. 
     // BUT, the order is reverse to what you'd expect! It's LTR.
     // This is due to the fact that `map` is an injected method of `F[A]`.
     (((x: Int) => x + 1) map {_ * 7} map {_ - 3}) (10),
     f,
     (f map {_ :+ 8})(List(13, 12)),
     List(1,2,3) >| "x",
     List(1,2,3) as "x",
     List(1,2,3).fpair,
     List(1,2,3).strengthL("x"),
     List(1,2,3).strengthR("x"),
     List(1,2,3).void
   )
}
