package com.fp.testing

import org.scalatest.Matchers

/**
 * Created by elkorn on 1/10/15.
 */
trait PropertyTesting extends Matchers {
  def testProperty(property: Prop): Unit =
    Prop.run(property) should be(Passed)
}
