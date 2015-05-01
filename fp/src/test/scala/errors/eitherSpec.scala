package fp.errors

import org.scalatest._

class EitherSpec extends FlatSpec with Matchers {
  import Errors._

  "try" should "wrap exceptions in a Left" in {
    Either.Try(sys.error("BLARGH")).toString should equal(Left(new RuntimeException("BLARGH")).toString)
  }

  "try" should "wrap results in a Right" in {
    Either.Try(5) should equal(Right(5))
  }

  "map" should "apply a mapping fn" in {
    Right(5).map(_ + 2) should equal(Right(7))
    Left(4).map(_.toString) should equal(Left(4))
  }

  "flatMap" should "apply a mapping fn and flatten nested results" in {
    def rightStr[T](x: T) = Right(x.toString)
    def leftStr[T](x: T) = Right(x.toString)
    Right(5).flatMap((x) => Right(x + 2)) should equal(Right(7))
    Right(5).flatMap((x) => Left(x + 2)) should equal(Left(7))
    Left(4).flatMap(rightStr) should equal(Left(4))
    Left(4).flatMap(leftStr) should equal(Left(4))
  }

  "orElse" should "supply a default value when dealing with Left" in {
    Left(4).orElse(Right(2)) should equal(Right(2))
    Right(4).orElse(Right(2)) should equal(Right(4))
  }

  "map2" should "map 2 eithers together" in {
    Right(4).map2(Right(3))(_ + _) should equal(Right(7))
    Left(4).map2(Right(3))((_, _).toString) should equal(Left(4))
    Right(4).map2(Left(3))((_, _).toString) should equal(Left(3))
  }

  "sequence" should "convert from Lit[Either] to Either[List]" in {
    Either.sequence(List(Right(2), Right(3), Right(4))) should equal(Right(List(2, 3, 4)))
    Either.sequence(List(Right(2), Left(3), Right(4))) should equal(Left(3))
  }

  "traverse" should "sequence a list, applying a mapping fn" in {
    (Either.traverse(List(2, 3, 4))((x) => Right(x + 2))) should equal(Right(List(4, 5, 6)))
    (Either.traverse(List(2, 3, 4))((x) => Left(x + 2))) should equal(Left(4))
  }

  "sequence2" should "work the same as sequence" in {
    Either.sequence2(List(Right(2), Right(3), Right(4))) should equal(Either.sequence(List(Right(2), Right(3), Right(4))))
    Either.sequence2(List(Right(2), Left(3), Right(4))) should equal(Either.sequence(List(Right(2), Left(3), Right(4))))

  }
}
