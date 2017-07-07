import chapter2.Fibonacci

/**
  * @author loustler
  * @since 07/07/2017 22:04
  */
class FibonacciTest extends App {
  val n = 5

  val result = Fibonacci fib n

  assert(n == 5)
}
