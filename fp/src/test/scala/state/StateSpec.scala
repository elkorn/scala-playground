package fp.state

import org.scalatest._
import org.scalatest.Matchers._

class StateSpec extends FlatSpec with Matchers {
  "unit" should "return a value regardless of state" in {
    State.unit(2).run(3) should equal((2, 3))
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
    n % 5

  }

  def stateOp1(n: Int) = {
    println(s"stateOp1: op2($n % 5)")
    op2(n % 5)
  }

  def stateOp2(n: Int) = {
    println(s"stateOp2: op4($n % 5)")
    op4(n % 5)
  }

  val expectedResultPipelineA = op1 _
  val expectedResultPipelineB = stateOp1 _ andThen op3
  val expectedStatePipeline = stateOp1 _ andThen stateOp2 _

  val state1 = State((s: Int) => (op1(s), stateOp1(s)))
  val state2 = State((s: Int) => (op3(s), stateOp2(s)))
  "map2" should "combine two state actions" in {
    val initialState = 12
    val expected = (
      mapOp(
        expectedResultPipelineA(initialState),
        expectedResultPipelineB(initialState)
      ),
        expectedStatePipeline(initialState)
    )
    State.map2(
      state1,
      state2
    )(mapOp).run(initialState) should equal(expected)

    state1.map2(state2)(mapOp).run(initialState) should equal(expected)
  }

  "map2_2" should "act the same as map2" in {
    val initialState = 12
    state1.map2_2(state2)(mapOp).run(initialState) should equal(state1.map2(state2)(mapOp).run(initialState))
  }

  "map2_3" should "act the same as map2" in {
    val initialState = 12
    state1.map2_3(state2)(mapOp).run(initialState) should equal(state1.map2(state2)(mapOp).run(initialState))
  }

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
    val s = State.map2(lattes, blackCoffees)(_ ::: _)
    s.run(CreditCard(20)) should equal((List(Latte, Latte, BlackCoffee), CreditCard(2)))
  }
}
