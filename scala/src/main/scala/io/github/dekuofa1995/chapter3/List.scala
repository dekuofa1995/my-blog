package io.github.dekuofa1995.chapter3

import io.github.dekuofa1995.common.StackSafe
import io.github.dekuofa1995.common.UnStackSafe

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def example1(): Unit = {
    val ex1: List[Double] = Nil
    val ex2: List[Int]    = Cons(1, Nil)
    val ex3: List[String] = Cons("a", Cons("b", Nil))
    println(ex1)
    println(ex2)
    println(ex3)
  }

  def work_3_1(): Unit = {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + sum(t)
      case _                                     => 101
    }
    println(x)
  }

  // 3.2 删除给定List的第一个元素
  def tail[A](ds: List[A]): List[A] =
    ds match {
      case Nil        => Nil // should throw an error?
      case Cons(_, t) => t
    }

  // 3.3 替换List中第一个元素
  def setHead[A](ds: List[A], h: A): List[A] =
    ds match {
      case Nil         => Cons(h, Nil) // throw error?
      case Cons(_, xs) => Cons(h, xs)
    }

  // 3.4 删除列表前 n 个元素
  @tailrec
  @StackSafe
  def drop[A](l: List[A], n: Int): List[A] = {
    // 忽略了n < 0 的参数异常情况
    if (n <= 0) l
    else
      l match {
        case Nil         => Nil // 忽略了删除长度超过列表总长的情况
        case Cons(_, xs) => drop(xs, n - 1)
      }

  }

  @annotation.tailrec
  @StackSafe
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _                  => l
    }

  @UnStackSafe
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  // 3.6 返回一个列表，新列表包含原列表除最后一个元素之外的所有元素
  @UnStackSafe
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil          => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs)  => Cons(x, init(xs))
    }

  // todo stack safe version

  //def sum(ints: List[Int]): Int =
  //  ints match {
  //    case Nil => 0
  //    case Cons(x, xs) => x + sum(xs)
  //  }

  //def product(ds: List[Double]): Double =
  //  ds match {
  //    case Nil => 1.0
  //    case Cons(x, xs) => x * product(xs)
  //  }

  @UnStackSafe
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @UnStackSafe
  def sum2(ints: List[Int]): Int =
    foldRight(ints, 0)(_ + _)

  @UnStackSafe
  def product2(ds: List[Double]): Double =
    foldRight(ds, 0.0)(_ * _)

  // 3.9 使用foldRight 计算长度
  @UnStackSafe
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, s) => s + 1)

  // foldLeft：将元素从左往右折叠(合并)为一个结果
  //    head => tail
  // foldRight: head <= tail
  // 3.10 实现尾递归的foldLeft，栈安全
  @StackSafe
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // 3.11 使用foldLeft 改写 sum、product、length
  def sumViaFoldLeft(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def productViaFoldLeft(l: List[Double]): Double =
    foldLeft(l, 0.0)(_ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0)((s, _) => s + 1)

  // 3.12 颠倒原列表的元素顺序
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((l, a) =>
      l match {
        case Nil => Cons(a, Nil)
        case _   => Cons(a, l)
    })

  def reverseViaFoldLeft[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((xs, x) => Cons(x, xs))

  @UnStackSafe
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((a, b) => f(b, a))

  @UnStackSafe
  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  // 使用foldRight 或者foldLeft 实现append
  def append2[A](l: List[A], r: List[A]): List[A] =
    foldRightViaFoldLeft(l, r)((ls, rs) => Cons(ls, rs))

  // 3.15 合并多个List
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  // 传递一个 List[Int] ，返回一个新的列表，其中元素是原列表元素+1
  def addViaFoldRight(ints: List[Int]): List[Int] =
    foldRight(ints, Nil: List[Int])((h, l) => Cons(h + 1, l))

  // 3.17 传入List[Double]，将每个元素转为string，返回新的List
  def double2String(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((d, nl) => Cons(d.toString, nl))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((a, b) => Cons(f(a), b))

  def add1_2(l: List[Int]): List[Int] =
    map(l)(x => x + 1)

  def filter[A](l: List[A])(p: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])(
      (a, t) =>
        if (p(a)) Cons(a, t)
        else t
    )

  // 将传入列表中的元素转为新的列表，合并后返回新列表
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((h, t) => append2(f(h), t))

  def flatMap_answer[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterViaFlatMap[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)(a => if (p(a)) List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, addPairwise(ta, tb))
    }

  def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] =
    (a, b) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt)(f))
    }

  def addPairwiseViaZipWith(a: List[Int], b: List[Int]): List[Int] =
    zipWith(a, b)(_ + _)

  @annotation.tailrec
  def startsWith[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (_, Nil)                                 => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
      case _                                        => false
    }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil                       => sub == Nil
      case _ if startsWith(sup, sub) => true // 从当前节点开始与 sub 列表匹配
      case Cons(_, t)                => hasSubsequence(t, sub) // 处理 sup 下一节点
    }

}
