package chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/**
  * @author loustler
  * @since 07/07/2017 23:22
  */
object List {
  def sum(x: List[Int]): Int = x match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(x: List[Double]): Double = x match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, y) => x * product(y)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}
