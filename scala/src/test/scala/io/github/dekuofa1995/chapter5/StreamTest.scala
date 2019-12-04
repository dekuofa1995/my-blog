package io.github.dekuofa1995.chapter5

import org.scalatest.FunSuite

class StreamTest extends FunSuite {
  test("5_14: Stream.startsWith") {
    assert(Stream(5).startsWith_2(Stream(5)))
    assert(Stream(1, 2, 3, 4).startsWith_2(Stream(1, 2, 3)))
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8).startsWith_2(Stream(1)))
    assert(!Stream(1).startsWith_2(Stream(1, 2, 3, 4, 5, 6, 7, 8)))
  }

  test("5_15: Stream.tails") {
    assert(Stream(5).tails.toString == Stream(Stream(5), Stream.empty).toString)
    assert(Empty.tails.toString == Stream(Stream.empty).toString)
    assert(
      Stream(1, 2, 3).tails.toString == Stream(Stream(1, 2, 3),
                                               Stream(2, 3),
                                               Stream(3),
                                               Empty).toString)
  }

}
