package chapter2

import scala.annotation.tailrec

/**
  * @author loustler
  * @since 07/07/2017 22:09
  */
object Sorted {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }
}
