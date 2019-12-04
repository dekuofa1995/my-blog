package io.github.dekuofa1995.chapter4

// 表示成功或者失败
sealed trait Either[+E, +A] {
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_)  => b
      case Right(_) => this
    }

  def mapViaFlatMap[EE >: E, B, C](b: Either[EE, B])(
      f: (A, B) => C
  ): Either[EE, C] =
    this flatMap (a => b map (bb => f(a, bb)))

  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(e)  => Left(e)
      case Right(v) => Right(f(v))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e)  => Left(e)
      case Right(v) => f(v)
    }

  def mapViaFlatMap_answer[EE >: E, B, C](b: Either[EE, B])(
      f: (A, B) => C
  ): Either[EE, C] =
    for {
      aa <- this // flatMap
      bb <- b    //map
    } yield f(aa, bb)

}

case class Left[+E](value: E) extends Either[E, Nothing]

// right 双关，即指右边也指成功、正确
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  //  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
  //    es match {
  //      case Nil => Right(Nil)
  //      case h :: t => h flatMap ((hh: A) => sequence(t) map ((tt: List[A]) => List(hh, tt)))
  //    }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil    => Right(Nil)
      case h :: t => (f(h) mapViaFlatMap traverse(t)(f))(_ :: _)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverseViaFoldRight(es)(e => e)

  // todo 重新理解
  def traverseViaFoldRight[E, A, B](as: List[A])(
      f: A => Either[E, B]
  ): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((h, t) =>
      (f(h) mapViaFlatMap t)(_ :: _))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    (mkName(name) mapViaFlatMap mkAge(age))(Person)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  sealed class Name(val value: String)

  sealed class Age(val value: Int)

  case class Person(name: Name, arg: Age)

  //  def mkPerson_2(name: String, age: Int): Either[List[String], Person] = {
  //
  //    val eitherName = mkName(name)
  //    val eitherAge  = mkAge(age)
  //    eitherName flatMap (n => eitherAge.flatMap())
  //  }

}
