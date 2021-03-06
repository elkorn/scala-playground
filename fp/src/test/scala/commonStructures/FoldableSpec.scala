package fp.commonStructures

import org.scalacheck.Properties
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.scalatest._
import fp.Lazy.Stream
import fp.functionalDataStructures.{ Tree, Branch, Leaf }
import scala.util.Random

object FoldableListSpecification extends Properties("Foldable list") {
  property("foldLeft") = forAll { (ns: List[Int]) => Foldable.list.foldLeft(ns)(0)(_ + _) == ns.sum }
  property("foldRight") = forAll { (ns: List[Int]) => Foldable.list.foldRight(ns)(0)(_ + _) == ns.sum }
  property("foldMap") = forAll { (ns: List[Int]) => Foldable.list.foldMap(ns)(_ + 1)(Monoid.intAddition) == ns.map(_ + 1).sum }
}

object FoldableIndexedSeqSpecification extends Properties("Foldable IndexedSeq") {
  property("foldLeft") = forAll { (ns: IndexedSeq[Int]) => Foldable.indexedSeq.foldLeft(ns)(0)(_ + _) == ns.sum }
  property("foldRight") = forAll { (ns: IndexedSeq[Int]) => Foldable.indexedSeq.foldRight(ns)(0)(_ + _) == ns.sum }
  property("foldMap") = forAll { (ns: IndexedSeq[Int]) => Foldable.indexedSeq.foldMap(ns)(_ + 1)(Monoid.intAddition) == ns.map(_ + 1).sum }
}

object FoldableStreamSpecification extends Properties("Foldable Stream") {
  property("foldLeft") = forAll { (ns: List[Int]) => Foldable.stream.foldLeft(Stream.apply(ns: _*))(0)(_ + _) == ns.toList.sum }
  property("foldRight") = forAll { (ns: List[Int]) => Foldable.stream.foldRight(Stream.apply(ns: _*))(0)(_ + _) == ns.toList.sum }
  property("foldMap") = forAll { (ns: List[Int]) => Foldable.stream.foldMap(Stream.apply(ns: _*))(_ + 1)(Monoid.intAddition) == ns.map(_ + 1).toList.sum }
}

object FoldableTreeSpecification extends Properties("Foldable Tree") {
  object GenTree {
    val random = new Random

    def tree: Gen[Tree[Int]] =
      Gen.choose(0, 100).flatMap {
        case x if x > 90 => // Branch both
          for {
            branchL <- tree
            branchR <- tree
          } yield Branch(branchL, branchR)
        case x if x > 60 => // Branch left
          tree.map(Branch(_, Leaf(random.nextInt)))
        case x if x > 30 => // Branch right
          tree.map(Branch(Leaf(random.nextInt), _))
        case x =>
          Gen.const(Leaf(random.nextInt))
      }
  }

  property("foldLeft") = forAll(GenTree.tree) { (ns: Tree[Int]) => Foldable.tree.foldLeft(ns)(0)(_ + _) == Tree.fold(ns)(a => a)(_ + _) }
  property("foldRight") = forAll(GenTree.tree) { (ns: Tree[Int]) => Foldable.tree.foldRight(ns)(0)(_ + _) == Tree.fold(ns)(a => a)(_ + _) }
  property("foldMap") = forAll(GenTree.tree) { (ns: Tree[Int]) => Foldable.tree.foldMap(ns)(_ + 1)(Monoid.intAddition) == Tree.fold(ns)(_ + 1)(_ + _) }
}

class FoldableOptionSpecification extends WordSpec with Matchers {
  "Option foldable" should {
    "fold left" in {
      Foldable.option.foldLeft(Some(12))(13)(_ + _) should equal(25)
      Foldable.option.foldLeft[Int, Int](None)(13)(_ + _) should equal(13)
    }

    "fold right" in {
      Foldable.option.foldRight(Some(12))(13)(_ + _) should equal(25)
      Foldable.option.foldRight[Int, Int](None)(13)(_ + _) should equal(13)
    }

    "fold map" in {
      Foldable.option.foldMap(Some(12))(_ + 13)(Monoid.intAddition) should equal(25)
      Foldable.option.foldMap[Int, Int](None)(_ + 13)(Monoid.intAddition) should equal(0)
    }

    "be toListable" in {
      Foldable.option.toList(Some(12)) should equal(List(12))
      Foldable.option.toList[Int](None) should equal(Nil: List[Int])
    }
  }

}
