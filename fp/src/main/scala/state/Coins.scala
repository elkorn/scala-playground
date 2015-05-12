package fp.state

sealed trait Input
sealed trait Output

case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

case object Candy extends Output

object MachineSimulation {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs.map(input => State.modify((machine: Machine) => (input, machine) match {
      case (_, Machine(_, 0, _)) => machine
      case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
      case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
      case _ => machine
    })))
    result <- State.get
  } yield (result.coins, result.candies)

  // TODO: Do a second simulation, which returns a list of Candies.
  def simulateMachine2(inputs: List[Input]): State[Machine, List[Output]] = ???
}
