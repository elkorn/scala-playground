import com.fpinscala.purestate.RNG
import com.fpinscala.testing.Gen

object run {
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

  prop.run(100, simple)
}