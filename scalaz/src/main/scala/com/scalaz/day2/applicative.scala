package com.scalaz.day2;

object ApplicativeTypeclass {
  import scalaz._;
  import Scalaz._;

  /*
    The Applicative typeclass extends Apply and introduces `point`.
    Point takes a value and puts it in some sort of default (or pure) context,
    a minimal context that still yields that value.
  */

 def show() =
   ("ApplicativeTypeclass",
     1.point[List],
     1.point[Option] map {_ + 18}
   )

 object ApplyTypeclass {
   /*
      Using ap, Apply enables <*>, *>, and <* operator.
      <*> takes a functor that has a function in it and another functor and 
      extracts that function from the first functor and then maps it over the
      second one. 
   */
   def show() = 
     ("ApplicativeTypeclass_Apply",
       9.some <*> {(_: Int) * 3}.some//,
       // 9.some *> {(_: Int) * 3}.some,
     )
 }
}
