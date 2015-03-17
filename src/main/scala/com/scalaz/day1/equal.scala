package com.scalaz.day1;

import scalaz._;
import Scalaz._;


object Equal {
  case class Test(t: String)
  def show() = (
    "Equal",
    1 === 1,
    // 1 === "foo"
    1.some =/= 2.some,
    // 1 assert_=== 2, // RuntimeException
    Test("aaa").some
    )
}
