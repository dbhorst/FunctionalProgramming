package Chapter03

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  // Execise 3.25
  "Size" should "return return the size of the tree" in {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.size(tree) should be(5)

    val tree2 = Branch(Leaf(1), Leaf(2))
    Tree.size(tree2) should be(3)

    // Exercise 3.29
    Tree.size2(tree) should be(5)
    Tree.size2(tree2) should be(3)
  }

  // Execise 3.26
  "Maximum" should "return return the max number in the tree" in {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.maximum(tree) should be(3)

    val tree2 = Branch(Branch(Leaf(4), Leaf(2)), Leaf(2))
    Tree.maximum(tree2) should be(4)

    val tree3 = Branch(Leaf(1), Leaf(2))
    Tree.maximum(tree3) should be(2)

    // Exercise 3.29
    Tree.maximum2(tree) should be(3)
    Tree.maximum2(tree2) should be(4)
    Tree.maximum2(tree3) should be(2)
  }

  // Execise 3.27
  "Depth" should "return return the max depth in the tree" in {
    val tree = Branch(Branch(Leaf(4), Leaf(5)), Leaf(6))
    Tree.depth(tree) should be(3)

    val tree2 = Branch(Branch(Branch(Leaf(4), Leaf(3)), Leaf(5)), Leaf(6))
    Tree.depth(tree2) should be(4)

    val tree3 = Branch(Leaf(1), Leaf(2))
    Tree.depth(tree3) should be(2)

    // Exercise 3.29
    Tree.depth2(tree) should be(3)
    Tree.depth2(tree2) should be(4)
    Tree.depth2(tree3) should be(2)
  }

  // Execise 3.28
  "Map" should "modifies each element in a tree with a given function" in {
    val tree = Branch(Branch(Leaf(4), Leaf(5)), Leaf(6))
    Tree.map(tree)(_ + 1) should be(Branch(Branch(Leaf(5), Leaf(6)), Leaf(7)))

    val tree2 = Branch(Branch(Branch(Leaf(4), Leaf(3)), Leaf(5)), Leaf(6))
    Tree.map(tree2)(_ + 1) should be(Branch(Branch(Branch(Leaf(5), Leaf(4)), Leaf(6)), Leaf(7)))

    val tree3 = Branch(Leaf(1), Leaf(2))
    Tree.map(tree3)(_ + 1) should be(Branch(Leaf(2), Leaf(3)))

    Tree.map2(tree)(_ + 1) should be(Branch(Branch(Leaf(5), Leaf(6)), Leaf(7)))
    Tree.map2(tree2)(_ + 1) should be(Branch(Branch(Branch(Leaf(5), Leaf(4)), Leaf(6)), Leaf(7)))
    Tree.map2(tree3)(_ + 1) should be(Branch(Leaf(2), Leaf(3)))
  }
}
