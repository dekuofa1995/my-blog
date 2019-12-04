package io.github.dekuofa1995.chapter5

import io.github.dekuofa1995.chapter5.Stream._

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

case object Empty extends Stream[Nothing]

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, _) => Some(h())
  }

  override def toString: String =
    this match {
      case Empty      => "Empty"
      case Cons(h, t) => s"Cons(${h()}, ${t().toString})"
    }

  def toList: List[A] =
    this match {
      case Empty      => Nil
      case Cons(h, t) => h() :: t().toList
    }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _                    => empty
    }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _                   => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _                    => empty
    }

  // 5.4 检测 Stream 中所有元素是否与给定断言匹配，遇到不匹配的值应立即终止
  def forAll(p: A => Boolean): Boolean =
    //    foldRight(true)((a, b) => p(a) && b)
    !exists(a => !p(a))

  //  def exists(p: A => Boolean): Boolean =
  //    this match {
  //      case Cons(h, t) => if (p(h())) true else t().exists(p)
  //      case _ => false
  //    }

  // 通过 foldRight 重写的 exists (可以提前终止)
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // 5.5 使用 foldRight 实现 takeWhile
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((h, t) => {
      // 此处t.takeWhileViaFoldRight 多余，foldRight 会根据情况，在需要的时候自动调用
      //        if (p(h)) cons(h, t.takeWhileViaFoldRight(p))
      if (p(h)) cons(h, t.takeWhileViaFoldRight(p))
      else empty
    })

  // (难)5.6 使用 foldRight 实现 headOption
  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  /* 5.7 使用 fold 实现 map、filter、append、flatMap，append 方法参数应该是非严格求值的*/
  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((h, t) => if (p(h)) cons(h, t) else t)

  // 注意 append 的方法泛型
  def append[B >: A](bs: => Stream[B]): Stream[B] =
    foldRight(bs)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  // 注意f的第二个参数为传名参数（惰性求值）
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

  // 5.13 使用 unfold 实现 map、take、takeWhile、zipWith以及zipAll
  def mapViaFold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(x, xs) => Some((f(x()), xs()))
      case _           => None
    }

  def takeViaFold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), m) if m > 0 => Some((h(), (t(), m - 1)))
      case _                        => None
    }

  def takeWhileViaFold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _                    => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _                            => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(Some(h1()) -> Some(h2()), t1() -> t2())
      case (Cons(h1, t1), _) => Some(Some(h1()) -> None, t1() -> empty)
      case (_, Cons(h2, t2)) => Some(None -> Some(h2()), empty -> t2())
      case _                 => None
    }

  def zipWithAll[B, C](s: Stream[B])(
      f: (Option[A], Option[B]) => C
  ): Stream[C] =
    unfold(this -> s) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(f(Some(h1()), Some(h2())) -> (t1(), t2()))
      case (Cons(h1, t1), _) =>
        Some(f(Some(h1()), Option.empty[B]) -> (t1(), empty))
      case (_, Cons(h2, t2)) =>
        Some(f(Option.empty[A], Some(h2())) -> (empty, t2()))
      case _ => None
    }

  // 难 5.14 使用已写过的函数实现 startsWith 函数
  // 简单版本
  def startsWith[A](s1: Stream[A]): Boolean =
    (this, s1) match {
      case (Empty, Empty) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() =>
        t1().startsWith(t2())
      case (Cons(_, _), _) => true
      case _               => false
    }

  // todo 理解
  def startsWith_2[A](s1: Stream[A]): Boolean =
    zipAll(s1) takeWhile (opt => opt._2.isDefined) forAll {
      case (h1, h2) => h1 == h2
    }

  // todo 理解
  // 5.15 使用 unfold 实现 tails 函数
  // 对于一个给定的 Stream，tails 返回这个 Stream 输入序列的所有后缀（包含原始 Stream）
  // 例如 Stream(1,2,3) 返回 Stream(Stream(1,2,3),Stream(2,3), Stream(3), Stream())
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s     => Some((s, s drop 1))
    } append Stream(empty)

  // todo 理解
  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith_2 s)

  // todo 5.16
}

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def check_5_4(): Unit = {
    assert(!Stream(1, 2, 3, 4, 5).forAll(i => { assert(i <= 4); i < 4 }))
    assert(Stream(1, 2, 3, 4, 5).forAll(i => { assert(i <= 6); i < 6 }))
    assert((empty: Stream[Int]).forAll(i => { assert(i <= 6); i < 6 }))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def check_5_5(): Unit = {
    assert(
      Stream(1, 2, 3, 4, 5).toString ==
        Stream(1, 2, 3, 4, 5)
          .takeWhileViaFoldRight(i => { assert(i <= 6); i < 6 })
          .toString
    )
    assert(
      Stream(1, 2, 3).toString ==
        Stream(1, 2, 3, 4, 5)
          .takeWhileViaFoldRight(i => { assert(i <= 4); i < 4 })
          .toString)
    assert(
      empty.toString ==
        (empty: Stream[Int])
          .takeWhileViaFoldRight(i => { assert(i <= 4); i < 4 })
          .toString)
  }

  def check_5_6(): Unit = {
    assert(Stream(1, 2, 3, 4, 5).headOption == Some(1))
    assert(Stream(1).headOption == Some(1))
    assert(empty.headOption == None)
  }

  def check_5_7_append(): Unit = {
    assert(
      Stream(1, 2)
        .append(Stream(3, 4, 5))
        .toString == Stream(1, 2, 3, 4, 5).toString)
    assert(Stream(1).append(empty).toString == Stream(1).toString)
    assert(empty.append(Stream(1)).toString == Stream(1).toString)
    assert(empty.append(empty).toString == empty.toString)
  }

  // 5.8 泛化 one 定义 constant，根据给定值返回一个无线流
  def constant[A](a: => A): Stream[A] =
    cons(a, constant(a))

  def check_5_8(): Unit = {
    assert(Stream(5, 5, 5, 5, 5).toString == constant(5).take(5).toString)
    assert(
      Stream(6, 6, 6, 6, 6).toString == constant(5).map(_ + 1).take(5).toString)
  }

  // 无限流与共递归
  //val ones: Stream[Int] = cons(1, ones)

  //example
  //ones.exists(_ % 2 == 1)
  //ones.map(_ + 1).exists(_ % 2== 0)
  //ones.forAll(_ != 1)
  //ones.takeWhile(_ == 1) // 注意该行代码并不会直接导致栈溢出

  // 5.9 写一个函数生成一个整数无线流，从n开始，然后n+1,n+2...
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def check_5_9(): Unit = {
    assert(Stream(5, 6, 7, 8, 9).toString == from(5).take(5).toString)
    assert(
      Stream(7, 8, 9, 10, 11).toString == from(5).map(_ + 2).take(5).toString)
  }

  // 5.10 写一个 fibs 函数生成斐波那契数列的无限流：0,1,1,2,3,5,8...
  def fibs(): Stream[Int] =
    fibs(0, 1)

  def fibs(x: Int, y: => Int): Stream[Int] = {
    cons(x, fibs(y, x + y))
  }

  def check_5_10(): Unit = {
    assert(
      Stream(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55).toString
        == fibs().takeWhile(_ <= 55).toString)
  }

  // 5.11 写一个更加通用的构造流的函数 unfold。
  // 接受一个初始状态，以及一个在生成的 Stream 中用于产生下一状态和下一个值的函数
  // todo 其中的 Option 是用来表示 Stream 何时结束
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def go(curr: Option[(A, S)]): Stream[A] =
      curr match {
        case Some((a, s)) => cons(a, go(f(s)))
        case None         => empty
      }

    go(f(z))
  }

  // todo check
  /* 5.12 使用 unfold 重写 fibs、from、constant、和ones */
  def fibsViaUnfold(): Stream[Int] =
    unfold(0, 1)(s => Some((s._1, (s._2, s._1 + s._2))))

  // answer
  //val fibs = {
  //  def go(f0: Int, f1: Int): Stream[Int] =
  //    cons(f0, go(f1, f0+f1))
  //  go(0, 1)
  //}

  def check_5_12_fibs(): Unit = {
    assert(
      Stream(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55).toString
        == fibsViaUnfold().takeWhile(_ <= 55).toString)
  }

  def fromViaUnfold(z: Int): Stream[Int] =
    unfold(z)(s => Some(s, s + 1))

  // answer
  // todo 内部函数所需入参列表为外部函数入参子集时，不用编写内部函数
  //def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  //  f(z) match {
  //    case Some((h,s)) => cons(h, unfold(s)(f))
  //    case None => empty
  //  }

  def check_5_12_from(): Unit = {
    assert(Stream(5, 6, 7, 8, 9).toString == fromViaUnfold(5).take(5).toString)
    assert(
      Stream(7, 8, 9, 10, 11).toString == fromViaUnfold(5)
        .map(_ + 2)
        .take(5)
        .toString)
  }

  //val fibsViaUnfold =
  //  unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  def check_5_12_constant(): Unit = {
    assert(
      Stream(5, 5, 5, 5, 5).toString == constantViaUnfold(5).take(5).toString)
    assert(
      Stream(6, 6, 6, 6, 6).toString == constantViaUnfold(5)
        .map(_ + 1)
        .take(5)
        .toString)
  }

  def onesViaUnfold(): Stream[Int] =
    //constant(1)
    unfold(1)(_ => Some(1, 1))

  def check_5_13_take(): Unit = {
    assert(Stream(5).takeViaFold(5).toString == Stream(5).toString)
    assert(empty.takeViaFold(5).toString == empty.toString)
    assert(
      Stream(6, 6, 6, 6, 6).takeViaFold(3).toString == Stream(6, 6, 6).toString)
  }

  def check_5_14(): Unit = {
    assert(Stream(5).startsWith_2(Stream(5)))
    assert(Stream(1, 2, 3, 4).startsWith_2(Stream(1, 2, 3)))
    assert(
      Stream(1, 2, 3, 4, 5, 6, 7, 8)
        .startsWith_2(Stream(1)))
    assert(!Stream(1).startsWith_2(Stream(1, 2, 3, 4, 5, 6, 7, 8)))
  }

  def main(args: Array[String]): Unit = {
    //cons({ println("head"); 1 }, empty).takeWhile(_ > 0)
    //check_5_4()
    //check_5_5()
    //check_5_6()
    //check_5_7_append()
    //check_5_8()
    //check_5_9()
    //check_5_10()
    //check_5_12_fibs()
    //check_5_12_from()
    //check_5_12_constant()
    //check_5_13_take()
    check_5_14()
  }

}
