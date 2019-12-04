package io.github.dekuofa1995.chapter2

import io.github.dekuofa1995.chapter3.Branch
import io.github.dekuofa1995.chapter3.Leaf
import io.github.dekuofa1995.chapter3.Tree._
import org.scalatest.FunSuite

class TreeTest extends FunSuite {
  test("3.25: size") {
    assert(1 == size(Leaf(1)))
    assert(3 == size(Branch(Leaf(1), Leaf(2))))
  }

  test("3.26: maximum") {
    assert(1 == maximum(Leaf(1)))
    assert(2 == maximum(Branch(Leaf(1), Leaf(2))))
    assert(3 == maximum(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))))
  }

  test("3.27: depth") {
    assert(0 == depth(Leaf(1)))
    assert(1 == depth(Branch(Leaf(1), Leaf(2))))
    assert(2 == depth(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))))
  }

  test("3.28: map") {
    assert(Leaf(2) == map(Leaf(1))(_ * 2))
    assert(Branch(Leaf(3), Leaf(4)) == map(Branch(Leaf(1), Leaf(2)))(_ + 2))
    assert(
      Branch(Leaf(-1), Branch(Leaf(1), Leaf(2))) == map(
        Branch(Leaf(1), Branch(Leaf(3), Leaf(4))))(_ - 2))
  }

  test("3.29: sizeViaFold") {
    assert(1 == sizeViaFold(Leaf(1)))
    assert(3 == sizeViaFold(Branch(Leaf(1), Leaf(2))))

  }

  test("3.29: maximumViaFold") {
    assert(1 == maximumViaFold(Leaf(1)))
    assert(2 == maximumViaFold(Branch(Leaf(1), Leaf(2))))
    assert(3 == maximumViaFold(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))))
  }

  test("3.29: depthViaFold") {
    assert(0 == depthViaFold(Leaf(1)))
    assert(1 == depthViaFold(Branch(Leaf(1), Leaf(2))))
    assert(2 == depthViaFold(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))))
  }

  test("3.29: mapViaFold") {
    assert(Leaf(2) == mapViaFold(Leaf(1))(_ * 2))
    assert(
      Branch(Leaf(3), Leaf(4)) == mapViaFold(Branch(Leaf(1), Leaf(2)))(_ + 2))
    assert(
      Branch(Leaf(-1), Branch(Leaf(1), Leaf(2))) == mapViaFold(
        Branch(Leaf(1), Branch(Leaf(3), Leaf(4))))(_ - 2))
  }
}
