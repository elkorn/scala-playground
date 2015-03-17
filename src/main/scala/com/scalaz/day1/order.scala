package com.scalaz.day1;

import scalaz._;
import Scalaz._;

object Order {
  /*
      Ord is for types that have an ordering. 
      Ord covers all the standard comparing 
      functions such as >, <, >= and <=.
  */

 def show() = (
   "Order",
   1 > 2.0,
   // 1 gt 2.0, // type mismatch
   1.0 ?|? 1.0,
   2.0 ?|? 1.0,
   1.0 ?|? 2.0,
   1.0 max 2.0
   )
}
