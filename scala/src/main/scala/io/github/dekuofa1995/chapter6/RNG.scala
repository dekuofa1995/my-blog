package io.github.dekuofa1995.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  // 6.3 生成
  // 一个 (Int, Double)对
  // 一个 (Double,Int) 对
  // 一个 (Double, Double, Double) 三元组
  // 复用已经写过的函数
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  // 6.2 生成 [0,1) 范围内的 Double 数
  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1)) -> r
  }

  // 生成一个非负随机数，注意需要处理 -Int.MinValue 溢出情况
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    (if (n < 0) -(n + 1) else n, r)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // 6.4 生成一组随机数
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count > 0) {
      val (x, r1) = rng.nextInt
      val (xs, r) = ints(count - 1)(r1)
      (x :: xs, r)
    } else {
      (List.empty[Int], rng)
    }
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val nextSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG  = SimpleRNG(nextSeed)
      val n        = (nextSeed >> 16).toInt
      (n, nextRNG)
    }

  }

}
