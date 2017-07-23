package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

/**
  * @author loustler
  * @since 07/16/2017 16:11
  */
object Tree {
  // exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v)      => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // exercise 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // exercise 3.29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l , r) => g(fold(l)(f)(g), fold(r)(f)(g)) // recursive with left and right.
  }

  // exercise 3.29
  def sizeWithFold[A](t: Tree[A]): Int = {
    fold[A, Int](t)(a => 1)((b1, b2) => 1 + b1 + b2)
  }

  // exercise 3.29
  def maximumWithFold(t: Tree[Int]): Int = {
    fold[Int, Int](t)(a => a)((b1, b2) => b1 max b2)
  }

  // exercise 3.29
  def depthWithFold[A](t: Tree[A]): Int = {
    fold[A, Int](t)(a => 0)((b1, b2) => 1 + (b1 max b2))
  }

  // exercise 3.29
  def mapWithFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold[A, Tree[B]](t)(a => Leaf(f(a)))((b1, b2) => Branch(b1, b2))
  }
}
