package io.github.dekuofa1995.chapter4

sealed trait Option[+A] {
  // answer_2
  def flatMap_2[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  // 如果Option不为None，对其应用f
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }

  // B >: A 表示B类型参数必须是A的父类型
  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(a) => a
    }

  // 不对ob求值，除非需要
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case None => ob
      case _ => this
    }

  // answer_2
  def orElse_2[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  // 如果值不满足f，转换Some为None
  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(a) if f(a) => this
      case _ => None
    }

  // answer_2
  def filter_2(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  // 如果Option不为None，对其应用f，可能会失败？
  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case None => None
      case Some(a) => f(a)
    }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def map2_2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  // 原Option列表如果包含None，则返回None，否则返回列表（使用Some包装）
  // todo 理解 sequence 思路
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  def check4_4(): Unit = {
    assert(Some(List(1, 2, 3)) == sequence(List(Some(1), Some(2), Some(3))))
    assert(None == sequence(List(Some(1), None, Some(3))))
  }

  // 提升函数，将普通入参的函数提升为 Option 参数，可以不用直接修改函数内容
  def lift[A, B](f: A => B): Option[A] => Option[B] =
    _ map f

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge     = Try {
      age.toInt
    }
    val optTickets = Try {
      numberOfSpeedingTickets.toInt
    }
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(x), Some(y)) => Some(f(x, y))
    }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    0.0 // todo

  def Try[A](a: => A): Option[A] =
    try Some(a) catch {
      case e: Exception => None // 丢掉了错误信息，需要改进
    }

  // 原Option列表如果包含None，则返回None，否则返回处理每个元素后的列表（使用Some包装）
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def check4_5(): Unit = {
    assert(Some(List(1, 2, 3)) == traverse(List("1", "2", "3"))(i => Try { i.toInt }))
    assert(None == traverse(List("1", "a", "3"))(i => Try { i.toInt }))
  }

  def main(args: Array[String]): Unit = {
    //check4_4()
    check4_5()
  }

}
