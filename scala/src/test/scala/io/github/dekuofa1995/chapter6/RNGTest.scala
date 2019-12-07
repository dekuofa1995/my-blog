package io.github.dekuofa1995.chapter6

import io.github.dekuofa1995.chapter6.RNG.SimpleRNG
import org.scalatest.FunSuite

class RNGTest extends FunSuite {

  test("6.1: nonNegativeInt") {
    var r: RNG = SimpleRNG(1)
    (1 to 100).foreach(_ => {
      val (n, rn) = RNG.nonNegativeInt(r)
      r = rn
      assert(n > 0)
    })
  }

  test("6.2: double") {
    var r: RNG = SimpleRNG(1)
    (1 to 100).foreach(_ => {
      val (n, rn) = RNG.double(r)
      r = rn
      assert(n >= 0 && n <= 1)
    })
  }

  test("rollDie") {
    assert(RNG.zero._1 == 0)
  }

}
