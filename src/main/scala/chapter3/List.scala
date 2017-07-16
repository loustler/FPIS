package chapter3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A] // tail is covariant

/**
  * @author loustler
  * @since 07/07/2017 23:22
  */
object List {
  def sum(x: List[Int]): Int = {
    foldLeft(x, 0)((n, m) => n + m)
  }

  def product(x: List[Double]): Double = {
    foldLeft(x, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`;
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def foldRight[A, B](x: List[A], z: B)(f: (A, B) => B): B = x match {
    case Nil        => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  // These is not quiz, but i think to need for test and etc..
  def size[A](x: List[A]): Int = {
    @tailrec
    def loop(i: Int, y: List[A]): Int = y match {
      case Nil          => i
      case Cons(h, Nil) => i + 1
      case Cons(h, t)   => loop(i + 1, t)
    }

    loop(0, x)
  }

  def getHead[A](x: List[A]): A = x match {
    case Nil =>
      sys.error("Can not find head element, cause the given list is Nil.")
    case Cons(h, _) => h
  }

  def isEmpty[A](x: List[A]): Boolean = x match {
    case Nil => true
    case _   => false
  }

  def has[A](x: List[A], e: A): Boolean = x match {
    case Nil          => sys.error("The given list is Nil")
    case Cons(h, Nil) => if (h == e) true else false
    case Cons(h, t)   => if (h == e) true else has(t, e)
  }

  // exercise 3.2
  def tail[A](x: List[A]): List[A] = x match {
    case Nil          => Nil
    case Cons(t, Nil) => Nil
    case Cons(t, h)   => h
  }

  // exercise 3.3
  def setHead[A](x: List[A], e: A): List[A] = x match {
    case Nil          => Cons(e, Nil)
    case Cons(t, Nil) => Cons(e, Nil)
    case Cons(t, h)   => Cons(e, h)
  }

  // exercise 3.4
  def drop[A](x: List[A], n: Int): List[A] = {
    @tailrec
    def loop(m: Int, y: List[A]): List[A] = y match {
      case Nil          => Nil
      case Cons(h, Nil) => if (m > 0) Nil else y
      case Cons(h, t)   => if (m > 0) loop(m - 1, t) else y
    }

    loop(n, x)
  }

  // exercise 3.5
  def dropWhile[A](x: List[A])(f: A => Boolean): List[A] = {
    @tailrec
    def loop(y: List[A]): List[A] = y match {
      case Nil          => Nil
      case Cons(h, Nil) => if (f(h)) Nil else y
      case Cons(h, t)   => if (f(h)) loop(t) else y
    }

    loop(x)
  }

  // exercise 3.6
  def init[A](x: List[A]): List[A] = x match {
    case Nil                   => Nil
    case Cons(h, Cons(e, Nil)) => Cons(h, Nil)
    case Cons(h, t)            => Cons(h, init(t))
  }

  // exercise 3.9
  def length[A](x: List[A]): Int = {
    foldLeft(x, 0)((m, n) => m + 1)
  }

  // exercise 3.10
  @tailrec
  def foldLeft[A, B](x: List[A], y: B)(f: (B, A) => B): B = x match {
    case Nil        => y
    case Cons(h, t) => foldLeft(t, f(y, h))(f)
  }

  // exercise 3.12
  def reverse[A](x: List[A]): List[A] = {
    foldLeft(x, Nil: List[A])((n, m) => Cons(m, n))
  }

  // exercise 3.13
  def foldRightWithFoldLeft[A, B](x: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(x, z)((b, a) => f(a, b))
  }

  // exercise 3.13, Its very awesome!
  def foldRightWithFoldLeftAndHOF[A, B](x: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(x, (b: B) => b)((v, a) => b => v(f(a, b)))(z)
  }

  // exercise 3.14
  def append[A](x: List[A], a: A): List[A] = {
    foldRight(x, Cons(a, Nil))((a, b) => Cons(a, b))
  }

  // exercise 3.1
  def flatten[A](x: List[List[A]]): List[A] = {
    foldRight(x, Nil: List[A])((x, t) => {
      foldRight(x, t)((xx, tt) => Cons(xx, tt))
    })
  }

  // exercise 3.15, it use foldLeft.  I think it is bad performance more than flatten. I'm not recommend to use it.
  def flattenByFoldLeft[A](x: List[List[A]]): List[A] = {
    reverse(foldLeft(x, Nil: List[A])((x, t) => {
      reverse(foldLeft(x, t)((tt, xx) => Cons(xx, tt)))
    }))
  }

  // exercise 3.16
  def increase(x: List[Int]): List[Int] = {
    foldRight(x, Nil: List[Int])((a, b) => Cons(a + 1, b))
  }

  // exercise 3.17
  def doubleToString(x: List[Double]): List[String] = {
    foldRight(x, Nil: List[String])((x, t) => Cons(x.toString, t))
  }

  // exercise 3.17, it use foldLeft
  def doubleToStringByFoldLeft(x: List[Double]): List[String] = {
    reverse(foldLeft(x, Nil: List[String])((t, x) => Cons(x.toString, t)))
  }

  // exercise 3.18
  def map[A, B](x: List[A])(f: A => B): List[B] = {
    foldRight(x, Nil: List[B])((x, t) => Cons(f(x), t))
  }

  // exercise 3.18, it use foldLeft
  def mapByFoldLeft[A, B](x: List[A])(f: A => B): List[B] = {
    reverse(foldLeft(x, Nil: List[B])((t, x) => Cons(f(x), t)))
  }

  // exercise 3.19
  def filter[A](x: List[A])(f: A => Boolean): List[A] = {
    foldRight(x, Nil: List[A])((x, t) => {
      if (f(x)) Cons(x, t)
      else t
    })
  }

  // exercise 3.19, It use foldLeft
  def filterByFoldLeft[A](x: List[A])(f: A => Boolean): List[A] = {
    reverse(
      foldLeft(x, Nil: List[A])((t, x) => {
        if (f(x)) Cons(x, t)
        else t
      })
    )
  }

  // exercise 3.20
  def flatMap[A, B](x: List[A])(f: A => List[B]): List[B] = {
    flatten(map(x)(f))
  }

  // exercise 3.21
  def filterByFlatMap[A](x: List[A])(f: A => Boolean): List[A] = {
    flatMap(x)(h => {
      if (f(h)) Cons(h, Nil)
      else Nil
    })
  }

  // exercise 3.22, It use pattern matching for pair list. Its awesome!
  def addPairwise(x: List[Int], y: List[Int]): List[Int] = (x, y) match {
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  // exercise 3.23
  def zipWith[A, B, C](x: List[A], y: List[B])(f: (A, B) => C): List[C] =
    (x, y) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  // exercise 3.24
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (Nil, Nil)        => false
      case (Nil, _)          => false
      case (_, Nil)          => false
      case (l, Cons(h, Nil)) => has(l, h)
      case (l, Cons(h, t))   => if (has(l, h)) true else hasSubsequence(l, t)
    }
}
