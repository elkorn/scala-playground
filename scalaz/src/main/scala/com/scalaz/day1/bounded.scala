package com.scalaz.day1;

import scalaz._;
import Scalaz._;

object Bounded {
  /*
      Bounded members have an upper and a lower bound.
      Scalaz uses Enums for that.
  */
  def show() = (
    "Bounded",
    implicitly[Enum[Char]].min,
    implicitly[Enum[Char]].max,
    implicitly[Enum[Long]].max,
    implicitly[Enum[Int]].min
    // implicitly[Enum[(Boolean, Int, Char)]].max,
    // implicitly[Enum[(Boolean, Int, Char)]].min
    )
}
