package io.github.dekuofa1995.chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // 3.25 统计一棵树中的节点数(叶子节点、分支节点)
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def check_3_25(): Unit = {
    assert(1 == size(Leaf(1)))
    assert(3 == size(Branch(Leaf(1), Leaf(2))))
  }

  // 3.26 返回Tree[Int]中的最大元素
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def check_3_26(): Unit = {
    assert(1 == maximum(Leaf(1)))
    assert(2 == maximum(Branch(Leaf(1), Leaf(2))))
    assert(3 == maximum(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))))
  }

  // 3.27 返回一棵树中从根节点到任何叶子节点最大的路径长度
  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  def check_3_27(): Unit = {
    assert(0 == depth(Leaf(1)))
    assert(1 == depth(Branch(Leaf(1), Leaf(2))))
    assert(2 == depth(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))))
  }

  // 3.28 map 接受一个函数，对树中每个元素进行修改
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  def check_3_28(): Unit = {
    assert(Leaf(2) == map(Leaf(1))(_ * 2))
    assert(Branch(Leaf(3), Leaf(4)) == map(Branch(Leaf(1), Leaf(2)))(_ + 2))
    assert(Branch(Leaf(-1), Branch(Leaf(1), Leaf(2))) == map(Branch(Leaf(1), Branch(Leaf(3), Leaf(4))))(_ - 2))
  }

  // 3.29 泛化 size、maximum、depth、map
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def size_2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximum_2(t: Tree[Int]): Int =
    fold(t)(v => v)(_ max _)

  def depth_2[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((l, r) => 1 + (l max r))

  def map_2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def check_3_29(): Unit = {
    assert(1 == size_2(Leaf(1)))
    assert(3 == size_2(Branch(Leaf(1), Leaf(2))))

    assert(1 == maximum_2(Leaf(1)))
    assert(2 == maximum_2(Branch(Leaf(1), Leaf(2))))
    assert(3 == maximum_2(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))))

    assert(0 == depth_2(Leaf(1)))
    assert(1 == depth_2(Branch(Leaf(1), Leaf(2))))
    assert(2 == depth_2(Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))))

    assert(Leaf(2) == map_2(Leaf(1))(_ * 2))
    assert(Branch(Leaf(3), Leaf(4)) == map_2(Branch(Leaf(1), Leaf(2)))(_ + 2))
    assert(Branch(Leaf(-1), Branch(Leaf(1), Leaf(2))) == map_2(Branch(Leaf(1), Branch(Leaf(3), Leaf(4))))(_ - 2))
  }

  def main(args: Array[String]): Unit = {
    //check_3_25()
    //check_3_26()
    //check_3_27()
    //check_3_28()
    check_3_29()
  }

}
