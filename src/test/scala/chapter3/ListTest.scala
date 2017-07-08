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
    
    val dropped = List.dropWhile(list, f)
    
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
}
