package io.github.dekuofa1995.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n       = (newSeed >> 16).toInt
    (n, nextRNG)
  }
}

object RNG {

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, r1) = rng.nextInt
    val (i2, r2) = r1.nextInt
    (i1 -> i2, r2)
  }

  // 6.1 生成一个非负数的随机数 [0, Int.MaxInt]
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    (if (n < 0) -(n + 1) else n, r)
  }

  //// 6.2 生成 [0,1) 范围内的 Double 数
  //def double(rng: RNG): (Double, RNG) = {
  //  val (n, r) = nonNegativeInt(rng)
  //  (n / (Int.MaxValue.toDouble + 1)) -> r
  //}

  //def double(rng: RNG): (Double, RNG) = {
  //  val (n, r) = nonNegativeInt(rng)
  //  (n / (Int.MaxValue.toDouble + 1), r)
  //}

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

  // 生成一个非负随机数，注意需要处理 -Int.MinValue 溢出情况
  //def nonNegativeInt(rng: RNG): (Int, RNG) = {
  //  val (n, r) = rng.nextInt
  //  (if (n < 0) -(n + 1) else n, r)
  //}

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
  //def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  //  if (count > 0) {
  //    val (x, r1) = rng.nextInt
  //    val (xs, r) = ints(count - 1)(r1)
  //    (x :: xs, r)
  //  } else {
  //    (List.empty[Int], rng)
  //  }
  //}

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt
  val zero           = rollDie(SimpleRNG(5))

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  val double: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  // 6.5 生成 [0,1) 范围内的 Double 数
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  // 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((ra, acc) => map2(ra, acc)(_ :: _))

  def ints(n: Int): Rand[List[Int]] =
    sequence(List.fill(n)(int))

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)
    }

  // 6.9 使用 flatMap 实现 map 和 map2
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { a =>
      unit(f(a))
    }

  // 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
      f: (A, B) => C
  ): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def rollDie: Rand[Int] = nonNegativeLessThan(6)

  def rollDie_Fix: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}
