package chapter4

import org.scalatest.FunSuite

/**
  * @author loustler
  * @since 08/12/2017 19:33
  */
class EitherTest extends FunSuite {
  test("The string either should be int either") {
    val value = "5"
    val e = Right(value)
    val expect = Right(value.toInt)

    val result = e.map(_.toInt)

    assert(expect == result)
  }

  test("The inner Either should be flatten") {
    val c = Right(5)
    val f = (n: Int) => Right(n)
    val expect = c

    val result = c.flatMap(f)

    assert(expect == result)
  }

  test("The Left either should be default either") {
    val dE = Right("Hello, world!")
    val lE = Left("The error occurred")
    val expect = dE

    val result = lE.orElse(dE)

    assert(expect == result)
  }

  test("The Right either should be not changed") {
    val dE = Right("Hello, world!")
    val lE = Right(3)
    val expect = Right(3)

    val result = lE.orElse(dE)

    assert(expect == result)
  }

  test("The 4 Either should be 44 Either") {
    val e = Right(4)
    val expect = Right(44)

    val result = e.map2(Right(11))((n, m) => n * m)

    assert(expect == result)
  }
}
