package io.github.dekuofa1995.chapter4

trait Partial[+E, +A] {

  def map[B](f: A => B): Partial[E, B] =
    this match {
      case Errors(es) => Errors(es)
      case Success(a) => Success(f(a))
    }

  def flatMap[EE >: E, B](f: A => Partial[EE, B]): Partial[EE, B] =
    this match {
      case Errors(es) => Errors(es)
      case Success(a) => f(a)
    }

  def orElse[EE >: E, B >: A](b: => Partial[EE, B]): Partial[EE, B] =
    this match {
      case Errors(_) => b
      case Success(a) => Success(a)
    }

  def map2[EE >: E, B, C](b: Partial[EE, B])(f: (A, B) => C): Partial[EE, C] =
    (this, b) match {
      case (Errors(es1), Errors(es2)) => Errors(es1 ++ es2)
      case (Errors(es), _) => Errors(es)
      case (_, Errors(es)) => Errors(es)
      case (Success(a), Success(b)) => Success(f(a, b))
    }

}

case class Errors[+E](get: Seq[E]) extends Partial[E, Nothing]

case class Success[+A](get: A) extends Partial[Nothing, A]

object Partial {

  def traverse[E, A, B](as: List[A])(f: A => Partial[E, B]): Partial[E, List[B]] =
    as match {
      case Nil => Success(Nil)
      case h :: t => (f(h) map2 traverse(t)(f)) (_ :: _)
    }
}
