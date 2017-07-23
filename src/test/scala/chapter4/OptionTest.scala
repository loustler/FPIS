package chapter4

import org.scalatest.FunSuite

/**
  * @author loustler
  * @since 07/23/2017 16:42
  */
class OptionTest extends FunSuite {
  val mean = (xs: Seq[Double]) =>
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  test("The result should be safety") {
    val xs = List[Double](1, 2, 3)

    val result = mean(xs)
    val expected = 1 to 3 / 3
  }

  test("The int option should be string option") {
    val v = 5
    val io = Some(v)

    val result = io.map(i => i.toString)
    val expected = Some(v.toString)

    assert(result == expected)
  }

  test("The none option should be return default value") {
    val dv = 5
    val no = None

    val result = no.getOrElse(dv)
    val expect = dv

    assert(result == dv)
  }

  test("The option that have inner option should be flatten") {
    val inner = Some(5)
    val option = Some(inner)

    val result = option.flatMap(s => s)
    val expected = inner

    val result2 = option.flatMap(s => Some(s.get + 5)) // It will Some(10)
    val expected2 = Some(10)

    assert(result equals expected) // Reference equality
    assert(result == expected) // Value equality

    assert(result2 equals expected2)
    assert(result2 == expected2)

    assert(!result.equals(expected2))
    assert(result != expected2)
  }

  test("The None should be some via orElse") {
    val none = None
    val op = Some(5)

    val result = none.orElse(op)
    val expected = op

    assert(result equals expected)
    assert(result == expected)
  }

  test("The some has 3 as value should be none by filter") {
    val some = Some(3)

    val result = some.filter(i => i > 5)
    val expected = None

    assert(result equals expected)
  }

  test("The 5 should be some(5)") {
    val v = 5
    val op = Option(v)

    assert(op.getOrElse(6) == v)
  }
}
