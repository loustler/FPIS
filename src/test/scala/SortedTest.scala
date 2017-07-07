import chapter2.Sorted

/**
  * @author loustler
  * @since 07/07/2017 22:14
  */
class SortedTest extends App {
  val arr = Array[Int](5, 6, 2, 1, 3, 4) // Not sorted
  val sort = (x: Int, y: Int) => if (x > y) true else false

  assert(Sorted.isSorted(arr, sort) == false)

  val sortedArr = arr.sortWith(sort)

  assert(Sorted.isSorted(sortedArr, sort) == true)
}
