package io.github.dekuofa1995.chapter2

import io.github.dekuofa1995.chapter3.List._
import io.github.dekuofa1995.chapter3.Cons
import io.github.dekuofa1995.chapter3.List
import io.github.dekuofa1995.chapter3.Nil
import org.scalatest.FunSuite

class ListTest extends FunSuite {
  test("3.2: tail") {
    assert(tail(List(1, 2, 3, 4)) == List(2, 3, 4))
    assert(tail(List(1)) == Nil)
    assert(tail(List(2, 3, 4)) == List(3, 4))
  }

  test("3.3: tail") {
    assert(setHead(List(1, 2, 3, 4), 0) == List(0, 2, 3, 4))
    assert(setHead(Nil, 1) == List(1))
    assert(setHead(List(1), 0) == List(0))
  }

  test("3.4: drop") {
    assert(drop(List(1, 2, 3, 4), 0) == List(1, 2, 3, 4))
    assert(drop(List(1, 2, 3, 4), 1) == List(2, 3, 4))
    assert(drop(List(1, 2, 3, 4), 4) == Nil)
    //drop(List(1, 2, 3, 4), 5) == Nil // throw error
  }

  test("3.5: dropWhile") {
    assert(dropWhile(List(1, 2, 3, 4), (x: Int) => x < 3) == List(3, 4))
    assert(dropWhile(List(1, 2, 3, 4), (x: Int) => x <= 1) == List(2, 3, 4))
    assert(dropWhile(List(1, 2, 3, 4), (x: Int) => x > 1) == List(1, 2, 3, 4))
  }

  test("3.6: init") {
    assert(List(1, 2, 3) == init(List(1, 2, 3, 4)))
    assert(List(1) == init(List(1, 2)))
    assert(Nil == init(List(1)))
    assert(Nil == init(Nil))
  }

  // todo 3.7
  // todo 3.8

  test("3.9: length") {
    assert(4 == length(List(1, 2, 3, 4)))
    assert(2 == length(List(1, 2)))
    assert(0 == length(Nil))
  }

  test("3.11: sumViaFoldLeft") {
    assert(sumViaFoldLeft(List(1, 2, 3, 4)) == sum2(List(1, 2, 3, 4)))
    assert(sumViaFoldLeft(List(0)) == sum2(List(0)))
    assert(sumViaFoldLeft(Nil) == sum2(Nil))
  }

  test("3.11: lengthViaFoldLeft") {
    assert(4 == lengthViaFoldLeft(List(1, 2, 3, 4)))
    assert(2 == lengthViaFoldLeft(List(1, 2)))
    assert(0 == lengthViaFoldLeft(Nil))
  }

  test("3.11: productViaFoldLeft") {
    assert(
      productViaFoldLeft(List(1, 2, 3, 4.5)) == product2(List(1, 2, 3, 4.5)))
    assert(productViaFoldLeft(List(1.1)) == product2(List(1.1)))
    assert(productViaFoldLeft(Nil) == product2(Nil))
  }

  test("3.12: reverseViaFoldLeft") {
    assert(List(4, 3, 2, 1) == reverseViaFoldLeft(List(1, 2, 3, 4)))
    assert(List(1, 2) == reverseViaFoldLeft(List(2, 1)))
    assert(List(1) == reverseViaFoldLeft(List(1)))
    assert(Nil == reverseViaFoldLeft(Nil))
  }

  test("3.15: concat") {
    println(concat(Cons(Cons(4, Nil), Cons(Cons(3, Nil), Nil))))
  }
  test("3.16: add") {
    assert(List(2, 3, 4, 5) == addViaFoldRight(List(1, 2, 3, 4)))
  }

  test("3.17: double2String") {
    assert(List("1.0", "2.0", "3.0", "4.0") == double2String(List(1, 2, 3, 4)))
  }

  test("3.18: add1_2") {
    assert(List(2, 3, 4, 5) == add1_2(List(1, 2, 3, 4)))
  }

  test("3.19: filter") {
    assert(List(3, 4) == filter(List(1, 2, 3, 4))(a => a > 2))
    assert(List(1, 3, 4) == filter(List(1, 2, 3, 4))(a => a != 2))
    assert(List(2) == filter(List(1, 2, 3, 4))(a => a == 2))
  }

  test("3.20: flatMap") {
    assert(List(1, 1, 2, 2, 3, 3) == flatMap(List(1, 2, 3))(i => List(i, i)))
  }

  test("3.21: filterViaFlatMap") {
    assert(List(3, 4) == filterViaFlatMap(List(1, 2, 3, 4))(a => a > 2))
    assert(List(1, 3, 4) == filterViaFlatMap(List(1, 2, 3, 4))(a => a != 2))
    assert(List(2) == filterViaFlatMap(List(1, 2, 3, 4))(a => a == 2))
  }

  test("3.22: addPairwise") {
    assert(List(3, 4) == addPairwise(List(1, 2), List(2, 2)))
    assert(Nil == addPairwise(List(1, 2), Nil))
    assert(Nil == addPairwise(Nil, List(2, 2)))
  }

  test("3.23: addPairwiseViaZipWith") {
    assert(List(3, 4) == addPairwiseViaZipWith(List(1, 2), List(2, 2)))
    assert(Nil == addPairwiseViaZipWith(List(1, 2), Nil))
    assert(Nil == addPairwiseViaZipWith(Nil, List(2, 2)))
  }

  test("3.24: hasSubsequence") {
    assert(hasSubsequence(List(1, 2), Nil))
    assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(4)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
  }
}
