package com.scalaz.day1;

import scalaz._;
import Scalaz._;

object Enum {
  /*
      Enum members are sequentially ordered types — 
      they can be enumerated. 
      The main advantage of the Enum typeclass is 
      that we can use its types in list ranges. 
      They also have defined successors and predecessors, 
      which you can get with the succ and pred functions.
  */
  def show() = (
    'a' to 'e', // NumericRange
    'a' |-> 'e', // List
    3 |=> 5, // Stream
    'B'.succ,
    1 -+- 'c',
    'b' --- 1,
    2 |--> (3, 8),
    2 |==> (3, 8)
    )
}
