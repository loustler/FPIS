package chapter2

/**
  * @author loustler
  * @since 07/07/2017 22:00
  */
object Fibonacci {
  def fib(n: Int): Int = {
    if (n <= 1) n
    else fib(n - 2) + fib(n - 1)
  }
}
