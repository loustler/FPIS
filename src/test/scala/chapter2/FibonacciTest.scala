package chapter2

import org.scalatest.FunSuite

/**
  * @author loustler
  * @since 07/07/2017 22:04
  */
class FibonacciSuite extends FunSuite {
  test("Fibonacci 5 should be 5") {
    val fibonacci = Fibonacci fib 5
    assert(fibonacci == 5)
  }
}
