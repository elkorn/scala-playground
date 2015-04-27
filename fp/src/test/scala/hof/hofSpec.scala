package fp.hof

import org.scalatest._

class HofSpec extends FlatSpec with Matchers {
  val add = (a: Int, b: Int) => a + b
  val div = (a: Int, b: Int) => a / b
  "factorial" should "compute the factorial value" in {
    Hof.factorial(4) should be(24)
    Hof.factorial(5) should be(120)
  }

  "Fib" should "compute Fibonacci numbers" in {
    Hof.fib(0) should be(0)
    Hof.fib(1) should be(1)
    Hof.fib(2) should be(1)
    Hof.fib(3) should be(2)
    Hof.fib(4) should be(3)
    Hof.fib(5) should be(5)
  }

  "isSorted" should "check if an array is sorted" in {
    Hof.isSorted[Int](List(1, 2, 3, 4, 5), _ > _) should be(false)
    Hof.isSorted[Int](List(1, 2, 3, 4), _ > _) should be(false)
    Hof.isSorted[Int](List(1, 2, 3, 4, 5), _ < _) should be(true)
    Hof.isSorted[Int](List(1, 2, 3, 4), _ < _) should be(true)
    Hof.isSorted[Int](Nil, _ < _) should be(true)
  }

  "curry" should "split a function into unary functions" in {
    (Hof.curry(add)(2)(5)) should be(add(2, 5))
  }

  "uncurry" should "reverse the effect of currying" in {
    (Hof.uncurry(Hof.curry(add))(2, 5)) should be(add(2, 5))
  }

  "andThen" should "work like compose with reversed function order" in {
    val add2 = Hof.curry(add)(2)
    val div5 = Hof.curry(div)(5)
    val composed = add2 compose div5
    val andThened = add2 andThen div5

    (composed(3)) should be(add2(div5(3)))
    (andThened(3)) should be(div5(add2(3)))
  }
}

