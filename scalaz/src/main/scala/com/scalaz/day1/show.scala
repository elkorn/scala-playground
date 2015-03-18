package com.scalaz.day1;

import scalaz._;
import Scalaz._;

object Show {
  /*
      Members of Show can be presented as strings.
  */
  def show() = (
    1.show, // type Cord - functional data structure for long strs
    1.shows,
    1.println
    )
}
