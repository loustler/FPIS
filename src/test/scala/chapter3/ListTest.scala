package chapter3

import org.scalatest.FunSuite

/**
  * @author loustler
  * @since 07/07/2017 23:39
  */
class ListTest extends FunSuite {
  test("The size of list should be 5") {
    val list = List(1, 2, 3, 4, 5)
    
    assert(List.size(list) == 5)
  }
  
  test("The head of list should be 4") {
    val list = List(4, 3, 2, 6, 7, 9, 7)
    
    assert(List.getHead(list) == 4)
    assert(!List.isEmpty(list))
  }
}
