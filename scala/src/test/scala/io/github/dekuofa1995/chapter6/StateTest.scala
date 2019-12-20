package io.github.dekuofa1995.chapter6

import io.github.dekuofa1995.chapter6.Machine._
import org.scalatest.FunSuite

class StateTest extends FunSuite {

  test("machine") {
    println(
      simulateMachine(
        List(Coin, Turn, Turn, Coin, Turn, Coin, Coin, Turn, Coin, Turn))
        .run(
          Machine(true, 5, 10)
        )
        ._1)
  }
}
