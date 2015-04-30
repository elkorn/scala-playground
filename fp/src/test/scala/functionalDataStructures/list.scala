package fp.functionalDataStructures

import org.scalatest._

class ListSpec extends FlatSpec with Matchers {
  "tail" should "return the tail of a list" in {
    val list = List.apply[Int](1, 2, 3)
    list.tail should equal(List(2, 3))
    list.tail.tail should equal(List(3))
    list.tail.tail.tail should be(Nil)
  }

  "setHead" should "change the head of a list" in {
    val list = List.apply[Int](1, 2, 3)
    List.setHead(list, 5) should equal(List(5, 2, 3))
  }

  "drop" should "remove a number of elements from a list" in {
    val list = List(1, 2, 3)
    list.drop(2) should equal(List(3))
    list.drop(3) should equal(Nil)
  }

  "dropWhile" should "remove elements from a list while a predicate is true" in {
    val list = List(1, 2, 3)
    List.dropWhile(list)(_ < 3) should equal(List(3))
    List.dropWhile(list)((x) => true) should equal(Nil)
  }

  "init" should "remove the last element from a list" in {
    List.init(List(1, 2, 3)) should equal(List(1, 2))
  }

  "foldRight" should "recreate a list by using Cons" in {
    List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) should equal(List(1, 2, 3))
  }

  "length" should "compute the length of a list" in {
    List.length(List(1, 2, 3)) should equal(3)
    List.length(Nil) should equal(0)
  }

  "foldLeft" should "have the same effect as foldRight when f is commutative" in {
    val list = List(1, 2, 3, 4, 5)
    List.foldLeft(list, 1)(_ * _) should equal(List.foldRight(list, 1)(_ * _))
  }

  "foldLeft" should "be used to compute sum, product and length" in {
    val list = List(1, 2, 3, 4, 5)
    List.foldLeft(list, 0)(_ + _) should equal(15)
    List.foldLeft(list, 1)(_ * _) should equal(120)
    List.foldLeft(list, 0)((b, a) => b + 1) should equal(5)
  }

  "reverse" should "reverse a list" in {
    List.reverse(List(1, 2, 3)) should equal(List(3, 2, 1))
  }

  "foldRigt2" should "work identically to foldRight" in {
    List.foldRight2(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) should equal(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
  }

  "concat" should "concatenate a list of lists" in {
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)
    val l3 = List(7, 8, 9)

    List.concat(List(l1, l2, l3)) should equal(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  "map" should "apply a mapping fn to all elements of a list" in {
    List.map(List(1, 2, 3))(_ + 1) should equal(List(2, 3, 4))
  }

  "flatMap" should "apply a mapping fn to all elements of a list and flattening the result" in {
    List.flatMap(List(1, 2, 3))((a) => List(a + 1, a + 2)) should equal(List(2, 3, 3, 4, 4, 5))
  }

  "zipWith" should "be able to add elements of two lists together" in {
    List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) should equal(List(5, 7, 9))
  }

  "hasSubsequence" should "find a subsequence within a list" in {
    List.hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3)) should be(true)
    List.hasSubsequence(List(1, 2, 3, 4, 5), List(1, 2)) should be(true)
    List.hasSubsequence(List(1, 2, 3, 4, 5), List(4, 5)) should be(true)
    List.hasSubsequence(List(1, 2, 3, 4, 5), List(5)) should be(true)
    List.hasSubsequence(List(1, 2, 3, 4, 5), List(1)) should be(true)
    List.hasSubsequence(List(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5)) should be(true)
  }
}

