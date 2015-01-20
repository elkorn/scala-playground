import com.fpinscala.monoids.Monoid
import com.fpinscala.testing.{Gen, Prop}

Monoid.string.op("abc", "def")
Monoid.list[Int].op(List(1, 2, 3), List(1, 2, 3))
Prop.run(Monoid.monoidLaws(Monoid.list[Int])(Gen.listOf(Gen.choose(-10, 10))))