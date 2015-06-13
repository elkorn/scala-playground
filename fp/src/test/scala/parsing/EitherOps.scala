package fp.parsing

import org.scalatest.Matchers

trait EitherOps {
  self: Matchers =>

  case class EitherOps[A, B](either: Either[A, B]) {
    def shouldFail = either shouldBe a[Left[_, _]]

    def shouldSucceedWith(expected: B) = either should equal(Right(expected))
  }

  implicit def ops[A, B](either: Either[A, B]) = EitherOps(either)
}
