package chapter2

import org.scalatest.FunSuite

/**
  * @author loustler
  * @since 07/07/2017 22:14
  */
class SortedTest extends FunSuite {
  val sort = (x: Int, y: Int) => if (x < y) true else false
  
  test("sorted array should be true") {
    val sortedArray = Array[Int](1, 2, 3, 4, 5)
    
    assert(Sorted.isSorted(sortedArray, sort) == true)
  }
  
  test("unsorted array should be false") {
    val unsortedArray = Array[Int](5, 3, 2, 1, 4)
    
    assert(Sorted.isSorted(unsortedArray, sort) == false)
  }
}
