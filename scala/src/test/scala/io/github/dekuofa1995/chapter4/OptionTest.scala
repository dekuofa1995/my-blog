package io.github.dekuofa1995.chapter4

import io.github.dekuofa1995.chapter4.Option._
import org.scalatest.FunSuite

class OptionTest extends FunSuite {

  test("4.4: sequence") {
    assert(Some(List(1, 2, 3)) == sequence(List(Some(1), Some(2), Some(3))))
    assert(None == sequence(List(Some(1), None, Some(3))))
  }

  test("4.5: traverse") {
    assert(Some(List(1, 2, 3)) == traverse(List("1", "2", "3"))(i =>
      Try { i.toInt }))
    assert(None == traverse(List("1", "a", "3"))(i => Try { i.toInt }))
  }
}
