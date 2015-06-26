package fp.commonStructures

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import fp.Lazy.Stream

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
