package chapter3

import org.scalatest.FunSuite

/**
  * @author loustler
  * @since 07/07/2017 23:39
  */
class ListTest extends FunSuite {
  test("The result sum all elements in list should be 15") {
    val list = List(1, 2, 3, 4, 5)

    assert(List.sum(list) == 15)
  }

  test("The result product all elements in list should be 120") {
    val list = List(1.0, 2.0, 3.0, 4.0, 5.0)

    assert(List.product(list) == 120)
  }

  test("The size of list should be 5") {
    val list = List(1, 2, 3, 4, 5)

    assert(List.size(list) == 5)
  }

  test("The head of list should be 4") {
    val list = List(4, 3, 2, 6, 7, 9, 7)

    assert(List.getHead(list) == 4)
    assert(!List.isEmpty(list))
  }

  test("The list of 3 elements dropped should be 4") {
    val list = List(1, 2, 3, 4, 5, 6, 7)

    val dropped = List.drop(list, 3)

    assert(!List.isEmpty(list))
    assert(List.size(list) == 7)
    assert(!List.isEmpty(dropped))
    assert(List.size(dropped) == 4)
  }

  test("The matched list should be 2") {
    val list = List(1, 2, 3, 4, 5, 6, 7)

    val f = (n: Int) => n <= 5

    val dropped = List.dropWhile(list)(f)

    assert(!List.isEmpty(list))
    assert(List.size(list) == 7)
    assert(!List.isEmpty(dropped))
    assert(List.size(dropped) == 2)
  }

  test("The iniited list should be 4") {
    val list = List(1, 2, 3, 4, 5)

    val initted = List.init(list)

    assert(!List.isEmpty(list))
    assert(List.size(list) == 5)
    assert(!List.isEmpty(initted))
    assert(List.size(initted) == 4)
  }

  test("The length should be 5") {
    val list = List(1, 2, 3, 4, 5)

    assert(List.length(list) == 5)
  }

  test("The result of sum should be 10") {
    val list = List(1, 2, 3, 4)
    val f = (x: Int, y: Int) => x + y

    assert(List.foldLeft(list, 0)(f) == 10)
  }

  test("The list should be reverse") {
    val list = List(1, 2, 3, 4)

    println(List.reverse(list))
  }

  test("The list should be sum") {
    val list = List(1, 2, 3, 4, 5)
    val f = (x: Int, y: Int) => x + y

    assert(List.foldRightWithFoldLeft(list, 0)(f) == 15)
  }

  test("The list should be sum with foldRightWithFoldLeftAndHOF") {
    val list = List(1, 2, 3, 4, 5)
    val f = (x: Int, y: Int) => x + y

    assert(List.foldRightWithFoldLeftAndHOF(list, 0)(f) == 15)
  }

  test("The list should be append") {
    val list = List(1, 2, 3, 4)
    val appended = List.append(list, 5)

    assert(List.size(list) == 4)
    assert(List.size(appended) == 5)
  }

  test("The list elements shoudl be increased") {
    val list = List(0, 1, 2, 3, 4)
    val increased = List.increase(list)

    println(increased)
    assert(!List.isEmpty(list))
    assert(!List.isEmpty(increased))
    assert(List.length(list) == 5)
    assert(List.length(increased) == 5)
  }
}
