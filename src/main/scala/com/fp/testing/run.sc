import com.fp.purestate.RNG
import com.fp.testing._

val simple = RNG.simple(3)
val (r1, rng1) = Gen.boolean.sample.run(simple)
val (r2, rng2) = Gen.boolean.sample.run(rng1)
val (r3, rng3) = Gen.boolean.sample.run(rng2)
Gen.unit(21).flatMap(x => Gen.unit[Int](x + 3)).sample.run(rng3)
Gen.unit(21).listOfN(Gen.choose(1, 5)).sample.run(rng3)._1
val intList = Gen.choose(0, 100).listOfN(10)
val prop =
  Gen.forAll(intList)(ns => ns.reverse.reverse == ns) &&
    Gen.forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
prop.run(100, 10, simple)
val smallInt = Gen.choose(-10, 10)
val maxProp = Gen.forAll(Gen.listOf(smallInt)) { ns =>
  val max = ns.max
  !ns.exists(_ > max)
}

Prop.run(maxProp)
val maxProp1 = Gen.forAll(Gen.listOf1(smallInt)) { ns =>
  val max = ns.max
  !ns.exists(_ > max)
}
Prop.run(maxProp1)
val takeWhileProp = Gen.forAll(Gen.genCogen(cogen)(Gen.boolean)) { fn =>
  Prop.run(Gen.forAll(intList) { ns =>
    ns.takeWhile(fn).forall(fn)
  }) == Passed
}
val cogen = new Cogen[Int] {
  def sample(a: Int, rng: RNG): RNG = {
    val (res, rng2) = rng.nextInt
    RNG.simple(a.toLong ^ res.toLong)
  }
}
val takeDropRelationship = Gen.forAll(Gen.genCogen(cogen)(Gen.boolean)) { fn =>
  Prop.run(Gen.forAll(intList) { ns =>
    ns.takeWhile(fn).dropWhile(fn) == Nil
  }) == Passed
}

def not[A](f: A => Boolean): A => Boolean =
  (a: A) => !f(a)
//  Prop.run(takeWhileProp && takeDropRelationship)

