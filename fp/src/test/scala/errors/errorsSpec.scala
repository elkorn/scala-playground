package fp.errors

import org.scalatest._

class ErrorsSpec extends FlatSpec with Matchers {
  import Errors._

  "mean" should "count the mean of a non-empty sequence" in {
    mean(List(1.0, 2.0, 3.0)) should equal(Some(2.0))
    mean(Nil) should equal(None)
  }

  "variance" should "count the variance of a non-empty sequence" in {
    variance(List(1.0, 2.0, 3.0)) should equal(Some(2.0 / 3))
    variance(Nil) should equal(None)
  }

  "lift" should "move any function to operate in the Option domain" in {
    lift(math.abs)(Some(-123)) should equal(Some(123))
    lift(math.abs)(None) should equal(None)
  }

  "try" should "convert exceptions to None" in {
    Try(sys.error("BLARH")) should equal(None)
  }

  "map2" should "map 2 options" in {
    map2(Some(1), Some(2))(_ + _) should equal(Some(3))
    map2(Some(1), None)(_ + _) should equal(None)
    map2(None, Some(1))((_, _) => 42) should equal(None)
    map2(None, None)((_, _) => 42) should equal(None)
  }

  "sequence" should "map List[Option] to Option[List]" in {
    sequence(List(Some(1), Some(2))) should equal(Some(List(1, 2)))
  }

  "traverse" should "apply a mapping fn and act as sequence" in {
    val list = List(1, 2, 3)
    traverse(list) {
      case 2 => None
      case x => Some(x)
    } should equal(None)
    traverse(list)((x) => Some(x + 2)) should equal(Some(List(3, 4, 5)))
  }

  "traverse2" should "work like traverse" in {
    val list = List(1, 2, 3)
    val failingFn: PartialFunction[Int, Option[Int]] = {
      case 2 => None
      case x => Some(x)
    }

    val successfulFn = (x: Int) => Some(x + 2)

    traverse2(list)(failingFn) should equal(traverse(list)(failingFn))
    traverse2(list)(successfulFn) should equal(traverse(list)(successfulFn))
  }

  "sequence2" should "work like sequence" in {
    val list = List(Some(1), Some(2))
    sequence2(list) should equal(sequence(list))
  }
}
