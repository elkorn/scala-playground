import com.fpinscala.adt.Tree
import com.fpinscala.adt.Tree.{Branch, Leaf}

val tree1 = Branch(
  Branch(
    Leaf(1),
    Branch(
      Branch(
        Leaf(18),
        Leaf(56)),
      Leaf(21))),
  Leaf(7))

tree1.size()
Tree.maximum(tree1)
tree1.depth()
tree1.map[Int](_ + 3)

Tree.fold(tree1, Nil: List[Int])((a, res) => a :: res)
Tree.maximum2(tree1)
Tree.size2(tree1)
