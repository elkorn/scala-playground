package fp.state

import org.scalatest._

class CoinsSpec extends FlatSpec with Matchers {
  import MachineSimulation._

  "an empty machine" should "not react to any input" in {
    val emptyMachine = Machine(false, 0, 0)
    simulateMachine(List(Coin, Coin, Turn)).run(emptyMachine) should equal(((0, 0), emptyMachine))
    simulateMachine(List(Turn, Turn, Coin)).run(emptyMachine) should equal(((0, 0), emptyMachine))
  }

  "inserting a coin into a locked machine" should "be accepted and unlock it" in {
    val machine = Machine(true, 1, 0)
    simulateMachine(List(Coin)).run(machine) should equal(((1, 1), Machine(false, 1, 1)))
  }

  "turning the knob on an unlocked machine" should "dispense a candy" in {
    val machine = Machine(false, 1, 0)
    simulateMachine(List(Turn)).run(machine) should equal(((0, 0), Machine(true, 0, 0)))
  }

  "the whole candy dispensing process" should "consist of inserting a coin and turning a knob" in {
    simulateMachine(List(Coin, Turn)).run(Machine(true, 1, 0)) should equal(((1, 0), Machine(true, 0, 1)))
    simulateMachine(List(Coin, Turn)).run(Machine(true, 10, 0)) should equal(((1, 9), Machine(true, 9, 1)))
  }

}
