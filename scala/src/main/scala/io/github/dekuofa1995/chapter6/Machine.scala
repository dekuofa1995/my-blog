package io.github.dekuofa1995.chapter6

import io.github.dekuofa1995.chapter6.State._
sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {}

object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    sequence(inputs.map(modify[Machine] _ compose update)).flatMap(_ =>
      get.map(s => (s.coins, s.candies)))

  def update(input: Input)(machine: Machine): Machine =
    (machine, input) match {
      case (Machine(true, candy, coin), Coin) if candy > 0 =>
        Machine(locked = false, candy, coin + 1)
      case (Machine(false, candy, coin), Turn) =>
        Machine(locked = true, candy - 1, coin)
      case (machine, _) => machine
    }

}

// v1
//object Machine {
//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
//    State(machine => {
//      val state: Machine = inputs.foldLeft(machine) {
//        case (Machine(true, candy, coin), Coin) if candy > 0 =>
//          Machine(locked = false, candy, coin + 1)
//        case (Machine(false, candy, coin), Turn) =>
//          Machine(locked = true, candy - 1, coin)
//        case (machine, _) => machine
//      }
//      ((state.coins, state.candies), state)
//    })
//}
