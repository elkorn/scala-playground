package fpinscala

import com.fpinscala.adt.Tree
import com.fpinscala.testing.{Gen, Passed, Prop}
import org.p99.scala.UnitSpec
import org.scalatest.Matchers

/**
 * Created by elkorn on 1/10/15.
 */
class ADTSpec extends UnitSpec with Matchers {
  it should "Generate a non-empty tree" in {
    val trees = Gen.genIntTree(Gen.unit(1))
    Prop.run(Gen.forAll(trees)(Tree.size2(_) > 0)) should be(Passed)
  }

  it should "Generate a tree according to given values" in {
    val values = Gen.choose(0, 5)
    val trees = Gen.genIntTree(values)
    Prop.run(Gen.forAll(trees)(Tree.fold(_, true)((v, result) => result && v >= 0 && v < 5))) should be(Passed)
  }
}
