package fp.functionalDataStructures

import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {
  "size" should "count the number of leaves and branches" in {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.size(tree) should equal(5)
  }

  "maximum" should "find the maximum value in a tree" in {
    val tree1 = Branch(Branch(Leaf(3), Leaf(2)), Leaf(3))
    val tree2 = Branch(Branch(Leaf(3), Leaf(5)), Leaf(1))
    Tree.maximum(tree1) should equal(3)
    Tree.maximum(tree2) should equal(5)
  }

  "depth" should "return the depth of a tree" in {
    val tree1 = Branch(Branch(Leaf(3), Leaf(2)), Leaf(3))
    val tree2 = Branch(Branch(Leaf(3), Branch(Leaf(1), Leaf(1))), Leaf(3))
    Tree.depth(tree1) should equal(2)
    Tree.depth(tree2) should equal(3)
  }

  "map" should "apply a mapping function to all nodes" in {
    val tree = Branch(Branch(Leaf(3), Leaf(2)), Leaf(3))
    Tree.map(tree)(_.toString) should equal(Branch(Branch(Leaf("3"), Leaf("2")), Leaf("3")))
  }
}

