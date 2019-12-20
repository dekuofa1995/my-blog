package io.github.dekuofa1995.chapter6

import io.github.dekuofa1995.chapter6.RNG._
import org.scalatest.FunSuite

class RNGTest extends FunSuite {

  test("6.1: nonNegativeInt") {
    var r: RNG = SimpleRNG(1)
    (1 to 10000000).foreach(_ => {
      val (n, rn) = RNG.nonNegativeInt(r)
      r = rn
      assert(n >= 0)
    })
  }

  test("6.2: double") {
    var r: RNG = SimpleRNG(1)
    (1 to 10000000).foreach(_ => {
      val (n, rn) = RNG.double(r)
      r = rn
      assert(n >= 0D && n < 1D)
    })
  }

  test("6.7.1: ints") {
    println(ints(100)(SimpleRNG(1L)).toString())
  }

}
