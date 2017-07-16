package chapter3

import org.scalatest.FunSuite

/**
  * @author loustler
  * @since 07/16/2017 16:28
  */
class TreeTest extends FunSuite {
  test("The size of list should be 7") {
    val tree = Branch(
      Branch(Leaf(5), Leaf(3)),
      Branch(Leaf(2), Leaf(1))
    )

    assert(Tree.size(tree) == 7)
  }

  test("The size of tree should be 1") {
    val tree = Leaf(3)

    assert(Tree.size(tree) == 1)
  }

  test("The maximum should be  44") {
    val tree = Branch(
      Branch(
        Branch(Leaf(3), Leaf(5)),
        Branch(Leaf(2), Leaf(3))
      ),
      Branch(
        Branch(Leaf(24), Leaf(40)),
        Branch(
          Leaf(35),
          Branch(
            Leaf(29),
            Leaf(44)
          )
        )
      )
    )
    val result = Tree.maximum(tree)

    assert(result == 44)
  }
}