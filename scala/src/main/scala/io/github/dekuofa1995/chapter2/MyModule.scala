package io.github.dekuofa1995.chapter2

import scala.annotation.tailrec

object MyModule {

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def fib(n: Int): Int = {
    @tailrec
    def go(i: Int, n: Int, prev: Int, now: Int): Int = {
      if (i <= n) go(i + 1, n, now, prev + now)
      else now
    }

    go(2, n, 0, 1)
  }

  def isNotSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    !isSorted(as, ordered)

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length <= 1) return true
    for (i <- 1 until as.length) {
      if (!ordered(as(i - 1), as(i)))
        return false
    }
    true
  }

  // 2.3 柯里化
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  // 2.4 反柯里化
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def main(args: Array[String]): Unit = {
    println(formatResult("factorial", 7, factorial))
    assert(isSorted(Array(1, 2, 3, 4, 5), (p: Int, n: Int) => p <= n))
    assert(!isSorted(Array(1, 2, 3, 5, 4), (p: Int, n: Int) => p <= n))
    assert(isSorted(Array(1, 2, 3, 5, 5), (p: Int, n: Int) => p <= n))
  }

  def factorial(n: Int): Int = {
    @tailrec
    def loop(i: Int, n: Int, r: Int): Int = {
      if (i <= n) loop(i + 1, n, r * i)
      else r
    }

    loop(1, n, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

}
