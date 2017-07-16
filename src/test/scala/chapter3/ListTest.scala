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

  test("The list should be flatten") {
    val list = List(List(1, 2, 3, 4, 5), List(6, 7, 8, 9, 10))
    val result = List.flatten(list)
    val expect = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    assert(!List.isEmpty(list))
    assert(!List.isEmpty(result))
    assertResult(result)(expect)
  }

  test("The list should be flatten By foldLeft") {
    val list = List(List(1, 2, 3, 4, 5), List(6, 7, 8, 9, 10))
    val result = List.flattenByFoldLeft(list)
    val expect = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    assert(!List.isEmpty(list))
    assert(!List.isEmpty(result))
    assertResult(result)(expect)
  }

  test("The list elements shoudl be increased") {
    val list = List(0, 1, 2, 3, 4)
    val increased = List.increase(list)

    assert(!List.isEmpty(list))
    assert(!List.isEmpty(increased))
    assert(List.length(list) == 5)
    assert(List.length(increased) == 5)
  }

  test("The double list should be change string list") {
    val list = List[Double](0, 1, 2, 3, 4, 5, 6, 7)
    val stringList = List.doubleToString(list)

    assert(!List.isEmpty(list))
    assert(!List.isEmpty(stringList))
    assert(List.getHead(stringList) != List.getHead(list))
    assert(!List.getHead(stringList).equals(List.getHead(list)))
    assert(List.getHead(stringList) == List.getHead(list).toString)
  }

  test("The double list should be change string list by foldLeft") {
    val list = List[Double](0, 1, 2, 3, 4, 5, 6, 7)
    val stringList = List.doubleToStringByFoldLeft(list)

    assert(!List.isEmpty(list))
    assert(!List.isEmpty(stringList))
    assert(List.getHead(stringList) != List.getHead(list))
    assert(!List.getHead(stringList).equals(List.getHead(list)))
    assert(List.getHead(stringList) == List.getHead(list).toString)
  }

  test("The Integer list should be string list") {
    val list = List(1, 2, 3, 4, 5, 6, 7, 8)
    val stringList = List.map(list)(x => x.toString)

    assert(!List.isEmpty(list))
    assert(!List.isEmpty(stringList))
    assert(!List.getHead(stringList).equals(List.getHead(list)))
    assert(List.getHead(stringList) == List.getHead(list).toString)
  }

  test("The Integer list should be string list by foldLeft") {
    val list = List(1, 2, 3, 4, 5, 6, 7, 8)
    val stringList = List.mapByFoldLeft(list)(x => x.toString)

    assert(!List.isEmpty(list))
    assert(!List.isEmpty(stringList))
    assert(!List.getHead(stringList).equals(List.getHead(list)))
    assert(List.getHead(stringList) == List.getHead(list).toString)
  }

  test("The Integer list to double list") {
    val list = List(1, 2, 3, 4, 5, 6, 7, 8)
    val doubleListByMap = List.map(list)(n => n.toDouble)
    val doubleList = List[Double](1, 2, 3, 4, 5, 6, 7, 8)

    assert(!List.isEmpty(list))
    assert(!List.isEmpty(doubleListByMap))
    assert(!List.getHead(doubleList).equals(List.getHead(list)))
    assert(List.getHead(doubleList) == List.getHead(list).toDouble)
    assertResult(doubleListByMap)(doubleList)
  }

  test("The Integer list to double list by foldLeft") {
    val list = List(1, 2, 3, 4, 5, 6, 7, 8)
    val doubleListByMap = List.mapByFoldLeft(list)(n => n.toDouble)
    val doubleList = List[Double](1, 2, 3, 4, 5, 6, 7, 8)

    assert(!List.isEmpty(list))
    assert(!List.isEmpty(doubleListByMap))
    assert(!List.getHead(doubleList).equals(List.getHead(list)))
    assert(List.getHead(doubleList) == List.getHead(list).toDouble)
    assertResult(doubleListByMap)(doubleList)
  }

  test("The integer list should be greater than 3") {
    val list = List(1, 2, 3, 4, 5, 6, 7, 8)
    val result = List.filter(list)(x => x > 3)
    val expect = List(4, 5, 6, 7, 8)

    assert(!List.isEmpty(list))
    assert(!List.isEmpty(result))
    assertResult(result)(expect)
  }

  test("The integer list should be greate than 3 by foldLeft") {
    val list = List(1, 2, 3, 4, 5, 6, 7, 8)
    val result = List.filterByFoldLeft(list)(x => x > 3)
    val expect = List(4, 5, 6, 7, 8)

    assert(!List.isEmpty(list))
    assert(!List.isEmpty(result))
    assertResult(result)(expect)
  }

  test("The list should be has same elements") {
    val list = List(1, 2, 3, 4, 5)
    val result = List.flatMap(list)(i => List(i, i))
    val expect = List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)

    assert(!List.isEmpty(list))
    assert(!List.isEmpty(result))
    assertResult(result)(expect)
  }
}
