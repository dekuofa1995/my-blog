package io.github.dekuofa1995.chapter5

import io.github.dekuofa1995.chapter5.Stream._
import org.scalatest.FunSuite

class StreamTest extends FunSuite {

  def newPrintNode[A](a: A): () => A =
    () => {
      lazy val b = a
      println(b.toString);
      b
    }

  test("5_2: Stream.take") {
    assert(Stream(1, 2, 3).toList == Stream(1, 2, 3, 4).take(3).toList)
    assert(Stream(1, 2, 3, 4).toList == Stream(1, 2, 3, 4).take(4).toList)
    assert(empty.toList == Stream(1, 2, 3, 4).take(0).toList)
    assert(empty.toList == Stream(1, 2, 3, 4).take(-1).toList)
  }

  test("5_2: Stream.drop") {
    assert(Stream(4).toList == Stream(1, 2, 3, 4).drop(3).toList)
    assert(Stream(3, 4).toList == Stream(1, 2, 3, 4).drop(2).toList)
    assert(Stream(1, 2, 3, 4).toList == Stream(1, 2, 3, 4).drop(0).toList)
    assert(Stream(1, 2, 3, 4).toList == Stream(1, 2, 3, 4).drop(-1).toList)
    assert(empty.toList == Stream(1, 2, 3, 4).drop(4).toList)
    assert(empty.toList == Stream(1, 2, 3, 4).drop(5).toList)
  }

  test("5_3: Stream.takeWhile") {
    assert(Stream(1, 2).toList == Stream(1, 2, 3, 4).takeWhile(_ < 3).toList)
    assert(Stream(1, 2, 3).toList == Stream(1, 2, 3, 4).takeWhile(_ < 4).toList)
    assert(
      Stream(1, 2, 3, 4)
        .map(_ * 2)
        .toList == Stream(1, 2, 3, 4).map(_ * 2).takeWhile(_ % 2 == 0).toList)

    assert(empty.toList == Stream(1, 2, 3, 4).takeWhile(_ > 2).toList)
    assert(empty.toList == Stream(1, 2, 3, 4).takeWhile(_ > 1).toList)
  }

  test("Stream.exists") {
    println("==== [1, 5].exists(_ < 3) = true ====")
    assert(Stream((1 to 5).map(newPrintNode): _*).exists(i => i() < 3))
    println("==== [1, 5].exists(_ > 3) = true ====")
    assert(Stream((1 to 5).map(newPrintNode): _*).exists(i => i() > 3))
    println("==== [1, 5].exists(_ > 6) = false ====")
    assert(!Stream((1 to 5).map(newPrintNode): _*).exists(i => i() > 6))
  }

  test("Stream.existsViaFoldRight") {
    println("==== [1, 5].existsViaFoldRight(_ < 3) = true ====")
    assert(
      Stream((1 to 5).map(newPrintNode): _*).existsViaFoldRight(i => i() < 3))
    println("==== [1, 5].existsViaFoldRight(_ > 3) = true ====")
    assert(
      Stream((1 to 5).map(newPrintNode): _*).existsViaFoldRight(i => i() > 3))
    println("==== [1, 5].existsViaFoldRight(_ > 6) = false ====")
    assert(
      !Stream((1 to 5).map(newPrintNode): _*).existsViaFoldRight(i => i() > 6))
  }

  test("5.4: Stream.forAll") {
    assert(Stream((1 to 5).map(newPrintNode): _*).forAll(i => i() < 6))
    assert(!Stream((1 to 5).map(newPrintNode): _*).forAll(i => i() < 5))
    assert(!Stream((1 to 5).map(newPrintNode): _*).forAll(i => i() < 2))
    assert(!Stream((1 to 5).map(newPrintNode): _*).forAll(i => i() < 1))
  }

  test("5.5 :Stream.takeWhileViaFoldRight") {
    assert(
      Stream((1 to 5).map(newPrintNode): _*)
        .map(i => i())
        .takeWhileViaFoldRight(i => i < 2)
        .toList == List(1))
    assert(
      Stream((1 to 5).map(newPrintNode): _*)
        .map(i => i())
        .takeWhileViaFoldRight(i => i < 1)
        .toList == List[Int]())
    assert(
      Stream((1 to 5).map(newPrintNode): _*)
        .map(i => i())
        .takeWhileViaFoldRight(i => i < 6)
        .toList == List(1, 2, 3, 4, 5))
  }

  test("5.6: Stream.headOptionViaFoldRight") {
    assert(
      Stream((1 to 5).map(newPrintNode): _*)
        .map(i => i())
        .headOptionViaFoldRight
        .contains(1))

    assert(
      Stream((5 to 10).map(newPrintNode): _*)
        .map(i => i())
        .headOptionViaFoldRight
        .contains(5))

    assert(empty.headOptionViaFoldRight.isEmpty)
  }

  test("5.7 (1): Stream.mapViaFoldRight") {
    assert(
      Stream((5 to 10).map(newPrintNode): _*)
        .mapViaFoldRight(i => i())
        .mapViaFoldRight(_ * 2)
        .toList == List((5 to 10).map(_ * 2): _*))

    assert(
      Stream((5 to 10).map(newPrintNode): _*)
        .mapViaFoldRight(i => i())
        .mapViaFoldRight(_ * 2)
        .takeWhileViaFoldRight(_ < 15)
        .toList == List((5 to 10).map(_ * 2) filter (_ < 15): _*))
  }

  test("5.7 (2): Stream.filterViaFoldRight") {
    assert(
      Stream((5 to 10).map(newPrintNode): _*)
        .mapViaFoldRight(i => i())
        .filterViaFoldRight(_ > 5)
        .takeWhileViaFoldRight(_ < 8)
        .toList == List(6 to 7: _*))

    assert(
      Stream((5 to 10).map(newPrintNode): _*)
        .mapViaFoldRight(i => i())
        .filterViaFoldRight(_ % 2 == 0)
        .takeWhileViaFoldRight(_ < 8)
        .toList == List(6 until 8 by 2: _*))
  }

  test("5.7 (3): Stream.appendViaFoldRight") {
    assert(
      ones.take(5).appendViaFoldRight(constant(2).take(5)).toList == List(
        (1, 5),
        (2, 5)).flatMap(x => (1 to x._2).map(_ => x._1)))
  }

  test("5.7 (4): Stream.flatMapViaFoldRight") {
    assert(
      Stream((1, 5), (2, 5))
        .flatMapViaFoldRight(i => constant(i._1).take(i._2))
        .toList ==
        List((1, 5), (2, 5)).flatMap(x => (1 to x._2).map(_ => x._1)))

    assert(
      Stream((newPrintNode(1), 5), (newPrintNode(2), 5))
        .flatMapViaFoldRight(i => constant(i._1).take(i._2))
        .map(i => i())
        .takeWhileViaFoldRight(_ < 2)
        .toList ==
        List((1, 5)).flatMap(x => (1 to x._2).map(_ => x._1)))
  }

  test("Stream.ones") {
    assert(ones.take(5).toList.length == 5)
    assert(ones.take(10).toList.length == 10)
    assert(ones.exists(_                   % 2 != 0))
    assert(ones.takeWhile(_ == 1).exists(_ % 2 != 0))

    // 栈溢出示例
    //ones.takeWhile(_ == 1).toList.length
  }

  test("5.8: Stream.constant") {
    assert(constant(1).take(5).toList.length == 5)
    assert(constant(1).take(10).toList.length == 10)
    assert(constant(1).exists(_                   % 2 != 0))
    assert(constant(1).takeWhile(_ == 1).exists(_ % 2 != 0))
  }

  test("5.9: Stream.from") {
    assert(from(1).take(5).toList == List(1, 2, 3, 4, 5))
    assert(from(1).filter(_ % 2 == 0).take(5).toList == List(2, 4, 6, 8, 10))
  }

  test("5.10: Stream.fibs") {
    assert(fibs().take(11).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55))
  }

  test("5.11: Stream.unfold") {
    assert(unfold(1)(s => Some((s, s))).take(5).toList == ones.take(5).toList)

    assert(
      unfold(1)(s => Some((s, s + 1))).take(5).toList == from(1).take(5).toList)

    assert(
      unfold((0, 1))(s => Some((s._1, s._2 -> (s._2 + s._1))))
        .take(20)
        .toList == fibs().take(20).toList)
  }

  test("5_12 (1): Stream.fibsViaUnfold") {
    assert(fibsViaUnfold().take(100).toList == fibs().take(100).toList)
  }

  test("5_12 (2): Stream.fromViaUnfold") {
    assert(fromViaUnfold(5).take(5).toList == List.range(5, 10))
    assert(fromViaUnfold(10).take(5).toList == List.range(10, 15))
  }

  test("5_12 (3): Stream.constantViaUnfold") {
    assert(constantViaUnfold(5).take(5).toList == List.fill(5)(5))
    assert(constantViaUnfold(10).take(5).toList == List.fill(5)(10))
  }

  test("5_12 (4): Stream.onesViaUnfold") {
    assert(onesViaUnfold.take(5).toList == ones.take(5).toList)
    assert(onesViaUnfold.take(10).toList == ones.take(10).toList)
  }

  test("5_13 (1): Stream.mapViaUnfold") {
    assert(
      Stream((1 to 5).map(newPrintNode): _*)
        .mapViaUnfold(i => i())
        .takeWhileViaFoldRight(_ < 3)
        .toList == List.range(1, 5).takeWhile(_ < 3))
  }

  test("5.13 (2): Stream.takeViaUnfold") {
    assert(
      constant(newPrintNode(1))
        .takeViaUnfold(5)
        .map(i => i())
        .toList == List.fill(5)(1))
  }

  test("5.13 (3): Stream.takeWhileViaUnfold") {
    assert(
      fibs()
        .takeWhileViaUnfold(_ < 100)
        .toList == fibs().takeWhile(_ < 100).toList)

    assert(
      fibs()
        .takeWhileViaUnfold(_ > 1)
        .toList == empty.toList)
  }

  test("5.13 (4): Stream.zipWithViaUnfold") {
    assert(
      from(1)
        .zipWith(ones)(_ + _)
        .take(5)
        .toList == List.range(2, 7)) //[2, 7)

    assert(
      from(1)
        .zipWith(from(1))(_ + _)
        .take(5)
        .toList == List.range(2, 12, 2))
  }

  test("5.14: Stream.zipAll") {
    assert(
      ones
        .take(10)
        .zipAll(ones.take(5))
        .map({
          case (Some(a), Some(b)) => a + b
          case (Some(a), _)       => a
          case (_, Some(b))       => b
          case _                  => empty
        })
        .toList == List(Seq.fill(5)(2) ++ Seq.fill(5)(1): _*))
  }

  test("5_14: Stream.startsWith") {
    assert(Stream(5).startsWith(Stream(5)))
    assert(Stream(1, 2, 3, 4).startsWith(Stream(1, 2, 3)))
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8).startsWith(Stream(1)))
    assert(!Stream(1).startsWith(Stream(1, 2, 3, 4, 5, 6, 7, 8)))
    assert(
      !Stream(1, 2, 3, 4, 5, 6, 7).startsWith(Stream(1, 2, 3, 4, 5, 6, 7, 8)))

    assert(
      Stream((1 to 10).map(newPrintNode): _*)
        .map(i => i())
        .startsWith(Stream(1 to 5: _*)))
  }

  test("5_14: Stream.startsWith_2") {
    assert(Stream(5).startsWith_2(Stream(5)))
    assert(Stream(1, 2, 3, 4).startsWith_2(Stream(1, 2, 3)))
    assert(Stream(1, 2, 3, 4, 5, 6, 7, 8).startsWith_2(Stream(1)))
    assert(!Stream(1).startsWith_2(Stream(1, 2, 3, 4, 5, 6, 7, 8)))
  }

  test("5_15: Stream.tailsViaUnfold") {
    assert(
      Stream(5).tailsViaUnfold.toString == Stream(Stream(5), Stream()).toString)
    assert(Stream().tailsViaUnfold.toString == Stream(Stream()).toString)
    assert(
      Stream(1, 2, 3).tailsViaUnfold.toString == Stream(Stream(1, 2, 3),
                                                        Stream(2, 3),
                                                        Stream(3),
                                                        Stream()).toString)
  }

  test("5_16: Stream.scanRight") {
    assert(Stream(1, 2, 3).scanRight(0)(_ + _).toList == List(6, 5, 3, 0))
    assert(Stream(1, 2, 3).scanRight(1)(_ * _).toList == List(6, 6, 3, 1))
    assert(
      Stream((1 to 5).map(newPrintNode): _*)
        .map(i => i())
        .scanRight(1)(_ * _)
        .toList == List(120, 120, 60, 20, 5, 1))

  }

}
