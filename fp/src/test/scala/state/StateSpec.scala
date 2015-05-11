package fp.state

import org.scalatest._
import org.scalatest.Matchers._
import State._

class StateSpec extends FlatSpec with Matchers {
  "unit" should "return a value regardless of state" in {
    unit(2).run(3) should equal((2, 3))
  }

  "map" should "apply a mapping fn to a state" in {
    State((s: Int) => (s + 3, s)).map(_ * 2).run(10) should equal((26, 10))
  }

  "flatMap" should "apply a mapping fn and flatten the result" in {
    // The first result (of type A) is being discarded, only the state transition persists through all the actions.
    State((s: Int) => (s + 3, s * 2)).flatMap((a) => State((s: Int) => (s / 3, s * 8))).run(10) should equal((6, 160))
  }

  def op1(n: Int): Int = {
    println(s"op1: $n + 3")
    n + 3
  }
  def op2(n: Int): Int = {
    println(s"op2: $n * 3")
    n * 3
  }
  def op3(n: Int): Int = {
    println(s"op3: $n + 1")
    n + 1
  }
  def op4(n: Int): Int = {
    println(s"op4: $n + 8")
    n + 8
  }

  def mapOp(a: Int, b: Int) = {
    println(s"mapOp: $a + $b")
    a + b
  }

  def stateOp(n: Int) = {
    println(s"stateOp: $n % 5")
    n + 31

  }

  def stateOp1(n: Int) = {
    println(s"stateOp1: op2($n % 5)")
    op2(n - 29)
  }

  def stateOp2(n: Int) = {
    println(s"stateOp2: op4($n % 5)")
    op4(n + 17)
  }

  val expectedResultPipelineA = op1 _
  val expectedResultPipelineB = stateOp1 _ andThen op3
  val expectedStatePipeline = stateOp1 _ andThen stateOp2 _

  val initialState = 12

  val state1 = State((s: Int) => (op1(s), stateOp1(s)))
  val state2 = State((s: Int) => (op3(s), stateOp2(s)))

  "map2" should "combine two state actions" in {
    val expected = (
      mapOp(
        expectedResultPipelineA(initialState),
        expectedResultPipelineB(initialState)
      ),
        expectedStatePipeline(initialState)
    )
    map2(
      state1,
      state2
    )(mapOp).run(initialState) should equal(expected)

    state1.map2(state2)(mapOp).run(initialState) should equal(expected)
  }

  "map2_2" should "act the same as map2" in {
    state1.map2_2(state2)(mapOp).run(initialState) should equal(state1.map2(state2)(mapOp).run(initialState))
  }

  "map2_3" should "act the same as map2" in {
    state1.map2_3(state2)(mapOp).run(initialState) should equal(state1.map2(state2)(mapOp).run(initialState))
  }

  "sequence" should "perform state transitions while keeping the intermediate results" in {
    val expectedResult1 = state1.run(initialState)
    val expectedResult2 = state2.run(expectedResult1._2)
    val expectedState = state1.map2(state2)((_, _) => 42).run(initialState)._2 // the result doesn't really matter here.
    sequence(List(state1, state2)).run(initialState) should equal(List(expectedResult1._1, expectedResult2._1), expectedState)
  }

  "sequence2" should "act the same as sequence" in {
    sequence2(List(state1, state2)).run(initialState) should equal(sequence(List(state1, state2)).run(initialState))
  }

  "sequence3" should "act the same as sequence" in {
    sequence3(List(state1, state2)).run(initialState) should equal(sequence(List(state1, state2)).run(initialState))
  }

  "get" should "promote the state value to a result" in {
    val expected = state2.run(initialState)
    get.run(state2.run(initialState)) should equal((expected, expected))
    get.run(initialState) should equal((initialState, initialState))
    (for {
      _ <- modify[Int](_ * 2)
      z <- get[Int]
    } yield z).run(initialState) should equal((initialState * 2, initialState * 2))
  }

  "set" should "lift any value to state value" in {
    set(123).run(initialState) should equal(((), 123))
    // This gets the modified state and then sets it to 13.
    (for {
      _ <- modify[Int](_ * 2)
      z <- get[Int]
      _ <- set(13)
    } yield z).run(initialState) should equal((initialState * 2, 13))
  }

  "modify" should "change the state value of a transition" in {
    (for {
      x <- state2
      y <- modify[Int](_ + 18)
    } yield y).run(initialState) should equal(((), state2.run(initialState)._2 + 18))
  }

  // EXAMPLES

  "coffee example" should "be more explanatory" in {
    val blackPrice = 4
    val lattePrice = 7
    trait Price {
      val price: Int
    }

    abstract class Order(val number: Int) extends Price

    case class BlackCoffeeOrder(override val number: Int) extends Order(number) {
      val price = blackPrice
    }

    case class LatteOrder(override val number: Int) extends Order(number) {
      val price = lattePrice
    }

    sealed trait Coffee
    case object BlackCoffee extends Coffee
    case object Latte extends Coffee
    case class CreditCard(balance: Int) {
      def charge(amount: Int) = CreditCard(balance - amount)
    }

    type Coffees = List[Coffee]

    def makeCoffees(order: Order): List[Coffee] = order match {
      case BlackCoffeeOrder(n) => List.fill(n)(BlackCoffee)
      case LatteOrder(n) => List.fill(n)(Latte)
    }

    def fulfillOrder(order: Order): State[CreditCard, Coffees] =
      State(creditCard => {
        println(order)
        println(creditCard)
        println(order.number)
        (makeCoffees(order), creditCard.charge(order.number * order.price))
      })

    fulfillOrder(LatteOrder(2)).run(CreditCard(20)) should equal((List(Latte, Latte), CreditCard(6)))
    val lattes: State[CreditCard, Coffees] = fulfillOrder(LatteOrder(2))
    val blackCoffees: State[CreditCard, Coffees] = fulfillOrder(BlackCoffeeOrder(1))
    val s = map2(lattes, blackCoffees)(_ ::: _)
    s.run(CreditCard(20)) should equal((List(Latte, Latte, BlackCoffee), CreditCard(2)))
  }
}
