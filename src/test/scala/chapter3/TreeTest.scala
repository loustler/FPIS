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

  test("The depth of tree should be 3") {
    val tree = Branch(
      Branch(
        Branch(
          Leaf(3),
          Leaf(6)
        ),
        Branch(
          Leaf(4),
          Leaf(2)
        )
      ),
      Branch(
        Branch(
          Leaf(1),
          Leaf(5)
        ),
        Branch(
          Leaf(7),
          Leaf(9)
        )
      )
    )

    assert(Tree.depth(tree) == 3)
  }

  test("The tree should be string") {
    val tree = Branch(
      Branch(
        Branch(
          Leaf(3),
          Leaf(6)
        ),
        Branch(
          Leaf(4),
          Leaf(2)
        )
      ),
      Branch(
        Branch(
          Leaf(1),
          Leaf(5)
        ),
        Branch(
          Leaf(7),
          Leaf(9)
        )
      )
    )

    val result = Tree.map(tree)(t => t.toString)

    val expected = Branch(
      Branch(
        Branch(
          Leaf("3"),
          Leaf("6")
        ),
        Branch(
          Leaf("4"),
          Leaf("2")
        )
      ),
      Branch(
        Branch(
          Leaf("1"),
          Leaf("5")
        ),
        Branch(
          Leaf("7"),
          Leaf("9")
        )
      )
    )

    assertResult(result)(expected)
  }

  test("The All of tree elements should be add") {
    val tree = Branch(
      Branch(
        Branch(
          Leaf(1),
          Leaf(2)
        ),
        Branch(
          Leaf(3),
          Branch(
            Leaf(4),
            Leaf(5)
          )
        )
      ),
      Branch(
        Branch(
          Leaf(6),
          Leaf(7)
        ),
        Leaf(8)
      )
    )

    val result = Tree.fold(tree)(a => a)((b1, b2) => b1 + b2)
    val expected = 1 to 8 sum

    assert(result == expected)
  }

  test("The max value of tree elements should be 10") {
    val maxValue = 10
    val tree = Branch(
      Branch(
        Branch(
          Leaf(9),
          Leaf(7)
        ),
        Branch(
          Leaf(maxValue),
          Leaf(8)
        )
      ),
      Branch(
        Branch(
          Leaf(3),
          Leaf(2)
        ),
        Branch(
          Leaf(5),
          Leaf(4)
        )
      )
    )

    val result = Tree.maximumWithFold(tree)
    val expected = maxValue

    assert(result == expected)
  }

  test("The depth of tree should be 4") {
    val tree = Branch(
      Branch(
        Branch(
          Leaf(3),
          Leaf(2)
        ),
        Leaf(4)
      ),
      Branch(
        Leaf(2),
        Branch(
          Leaf(3),
          Branch(
            Leaf(2),
            Leaf(10)
          )
        )
      )
    )

    val result = Tree.depthWithFold(tree)
    val expected = 4

    assert(result == expected)
  }

  test("The int tree should be string tree") {
    val tree = Branch(
      Branch(
        Branch(
          Leaf(3),
          Leaf(6)
        ),
        Branch(
          Leaf(4),
          Leaf(2)
        )
      ),
      Branch(
        Branch(
          Leaf(1),
          Leaf(5)
        ),
        Branch(
          Leaf(7),
          Leaf(9)
        )
      )
    )

    val result = Tree.map(tree)(t => t.toString)

    val expected = Branch(
      Branch(
        Branch(
          Leaf("3"),
          Leaf("6")
        ),
        Branch(
          Leaf("4"),
          Leaf("2")
        )
      ),
      Branch(
        Branch(
          Leaf("1"),
          Leaf("5")
        ),
        Branch(
          Leaf("7"),
          Leaf("9")
        )
      )
    )

    assertResult(result)(expected)
  }
}
