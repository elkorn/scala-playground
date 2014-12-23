import com.fpinscala.purestate.RNG
import com.fpinscala.testing.Gen

object run {
  //  val intList = Gen.listOf(Gen.choose(0, 100))
  //  val prop =
  //    forAll(intList)(ns => ns.reverse.reverse == ns) &&
  //      forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
  val simple = RNG.simple(System.currentTimeMillis())
  Gen.boolean.sample.run(simple)._1
}