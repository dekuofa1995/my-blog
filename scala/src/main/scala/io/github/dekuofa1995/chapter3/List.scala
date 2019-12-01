package io.github.dekuofa1995.chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
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
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(x)
  }

  // 3.2 删除给定List的第一个元素
  def tail[A](ds: List[A]): List[A] =
    ds match {
      case Nil => Nil // should throw an error?
      case Cons(_, t) => t
    }

  def check_3_2(): Unit = {
    assert(tail(List(1, 2, 3, 4)) == List(2, 3, 4))
    assert(tail(List(1)) == Nil)
    assert(tail(List(2, 3, 4)) == List(3, 4))
  }

  // 3.3 替换List中第一个元素
  def setHead[A](ds: List[A], h: A): List[A] =
    ds match {
      case Nil => Cons(h, Nil) // throw error?
      case Cons(_, xs) => Cons(h, xs)
    }

  def check_3_3(): Unit = {
    assert(setHead(List(1, 2, 3, 4), 0) == List(0, 2, 3, 4))
    assert(setHead(Nil, 1) == List(1))
    assert(setHead(List(1), 0) == List(0))
  }

  // 3.4 删除列表前 n 个元素
  @tailrec
  @StackSafe
  def drop[A](l: List[A], n: Int): List[A] = {
    // 忽略了n < 0 的参数异常情况
    if (n <= 0) l
    else l match {
      case Nil => Nil // 忽略了删除长度超过列表总长的情况
      case Cons(_, xs) => drop(xs, n - 1)
    }

  }

  def check_3_4(): Unit = {
    assert(drop(List(1, 2, 3, 4), 0) == List(1, 2, 3, 4))
    assert(drop(List(1, 2, 3, 4), 1) == List(2, 3, 4))
    assert(drop(List(1, 2, 3, 4), 4) == Nil)
    //drop(List(1, 2, 3, 4), 5) == Nil // throw error
  }

  @scala.annotation.tailrec
  @StackSafe
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def check_3_5(): Unit = {
    assert(dropWhile(List(1, 2, 3, 4), (x: Int) => x < 3) == List(3, 4))
    assert(dropWhile(List(1, 2, 3, 4), (x: Int) => x <= 1) == List(2, 3, 4))
    assert(dropWhile(List(1, 2, 3, 4), (x: Int) => x > 1) == List(1, 2, 3, 4))
  }

  @UnStackSafe
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  // 3.6 返回一个列表，新列表包含原列表除最后一个元素之外的所有元素
  @UnStackSafe
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  // todo stack safe version


  def check_3_6(): Unit = {
    assert(List(1, 2, 3) == init(List(1, 2, 3, 4)))
    assert(List(1) == init(List(1, 2)))
    assert(Nil == init(List(1)))
    assert(Nil == init(Nil))
  }

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
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @UnStackSafe
  def sum2(ints: List[Int]): Int =
    foldRight(ints, 0)(_ + _)

  @UnStackSafe
  def product2(ds: List[Double]): Double =
    foldRight(ds, 0.0)(_ * _)

  // todo 3.7
  // todo 3.8

  // 3.9 使用foldRight 计算长度
  @UnStackSafe
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, s) => s + 1)

  def check_3_9(): Unit = {
    assert(4 == length(List(1, 2, 3, 4)))
    assert(2 == length(List(1, 2)))
    assert(0 == length(Nil))
  }

  // foldLeft：将元素从左往右折叠(合并)为一个结果
  //    head => tail
  // foldRight: head <= tail
  // 3.10 实现尾递归的foldLeft，栈安全
  @StackSafe
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // 3.11 使用foldLeft 改写 sum、product、length
  def sum3(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double =
    foldLeft(l, 0.0)(_ * _)

  def length3[A](l: List[A]): Int =
    foldLeft(l, 0)((s, _) => s + 1)

  def check_3_11(): Unit = {
    assert(4 == length3(List(1, 2, 3, 4)))
    assert(2 == length3(List(1, 2)))
    assert(0 == length3(Nil))

    assert(sum3(List(1, 2, 3, 4)) == sum2(List(1, 2, 3, 4)))
    assert(sum3(List(0)) == sum2(List(0)))
    assert(sum3(Nil) == sum2(Nil))

    assert(product3(List(1, 2, 3, 4.5)) == product2(List(1, 2, 3, 4.5)))
    assert(product3(List(1.1)) == product2(List(1.1)))
    assert(product3(Nil) == product2(Nil))
  }

  // 3.12 颠倒原列表的元素顺序
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])(
      (l, a) =>
        l match {
          case Nil => Cons(a, Nil)
          case _ => Cons(a, l)
        })

  def check_3_12(): Unit = {
    assert(List(4, 3, 2, 1) == reverse(List(1, 2, 3, 4)))
    assert(List(1, 2) == reverse(List(2, 1)))
    assert(List(1) == reverse(List(1)))
    assert(Nil == reverse(Nil))
  }


  @UnStackSafe
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((a, b) => f(b, a))

  // todo 理解 foldRightViaFoldLeft_1
  @UnStackSafe
  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)


  // 使用foldRight 或者foldLeft 实现append
  def append2[A](l: List[A], r: List[A]): List[A] =
    foldRightViaFoldLeft(l, r)((ls, rs) => Cons(ls, rs))

  // 3.15 合并多个List
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  def check_3_15(): Unit = {
    println(concat(Cons(Cons(4, Nil), Cons(Cons(3, Nil), Nil))))
  }

  // 传递一个 List[Int] ，返回一个新的列表，其中元素是原列表元素+1
  def add1(ints: List[Int]): List[Int] =
    foldRight(ints, Nil: List[Int])((h, l) => Cons(h + 1, l))

  def check_3_16(): Unit = {
    assert(List(2, 3, 4, 5) == add1(List(1, 2, 3, 4)))
  }

  // 3.17 传入List[Double]，将每个元素转为string，返回新的List
  def double2String(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((d, nl) => Cons(d.toString, nl))

  def check_3_17(): Unit = {
    assert(List("1.0", "2.0", "3.0", "4.0") == double2String(List(1, 2, 3, 4)))
  }

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((a, b) => Cons(f(a), b))

  def add1_2(l: List[Int]): List[Int] =
    map(l)(x => x + 1)

  def check_3_18(): Unit = {
    assert(List(2, 3, 4, 5) == add1_2(List(1, 2, 3, 4)))
  }

  def filter[A](l: List[A])(p: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])(
      (a, t) =>
        if (p(a)) Cons(a, t)
        else t
      )

  def check_3_19(): Unit = {
    assert(List(3, 4) == filter(List(1, 2, 3, 4))(a => a > 2))
    assert(List(1, 3, 4) == filter(List(1, 2, 3, 4))(a => a != 2))
    assert(List(2) == filter(List(1, 2, 3, 4))(a => a == 2))
  }

  // 将传入列表中的元素转为新的列表，合并后返回新列表
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((h, t) => append2(f(h), t))

  def flatMap_answer[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def check_3_20(): Unit = {
    assert(List(1, 1, 2, 2, 3, 3) == flatMap(List(1, 2, 3))(i => List(i, i)))
  }

  def filter_2[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)(a => if (p(a)) List(a) else Nil)

  def check_3_21(): Unit = {
    assert(List(3, 4) == filter_2(List(1, 2, 3, 4))(a => a > 2))
    assert(List(1, 3, 4) == filter_2(List(1, 2, 3, 4))(a => a != 2))
    assert(List(2) == filter_2(List(1, 2, 3, 4))(a => a == 2))
  }

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, addPairwise(ta, tb))
    }

  def check_3_22(): Unit = {
    assert(List(3, 4) == addPairwise(List(1, 2), List(2, 2)))
    assert(Nil == addPairwise(List(1, 2), Nil))
    assert(Nil == addPairwise(Nil, List(2, 2)))
  }

  def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt)(f))
    }

  def addPairwise_2(a: List[Int], b: List[Int]): List[Int] =
    zipWith(a, b)(_ + _)

  def check_3_23(): Unit = {
    assert(List(3, 4) == addPairwise_2(List(1, 2), List(2, 2)))
    assert(Nil == addPairwise_2(List(1, 2), Nil))
    assert(Nil == addPairwise_2(Nil, List(2, 2)))
  }

  @scala.annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) =>
        if (h1 == h2) hasSubsequence(t1, t2)
        else hasSubsequence(t1, sub)
    }

  def check_3_24(): Unit = {
    assert(hasSubsequence(List(1, 2), Nil))
    assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(4)))
    assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
  }


  def main(args: Array[String]): Unit = {
    //example1()
    //work_3_1()
    //check_3_2()
    //check_3_3()
    //check_3_4()
    //check_3_5()
    //check_3_6()
    //check_3_9()
    //check_3_11()
    //check_3_12()
    //check_3_15()
    //check_3_16()
    //check_3_17()
    //check_3_18()
    //check_3_19()
    //check_3_20()
    //check_3_21()
    //check_3_22()
    //check_3_23()
    check_3_24()
  }
}
