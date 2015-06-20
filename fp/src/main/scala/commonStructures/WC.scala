package fp.commonStructures

import fp.state.parallelism.parallelism.Par
import fp.state.parallelism.Par

sealed trait WC

object WC {
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def monoid = new Monoid[WC] {
    def op(wc1: WC, wc2: WC) = (wc1, wc2) match {
      case (Stub(ch1), Stub(ch2)) => Stub(ch1 + ch2)
      case (Stub(lch), Part(lStub, words, rStub)) => Part(lch + lStub, words, rStub)
      case (Part(lStub, words, rStub), Stub(rch)) => Part(lStub, words, rStub + rch)
      case (Part(lStub1, words1, rStub1), Part(lStub2, words2, rStub2)) =>
        Part(lStub1, words1 + words2 + 1, rStub2)
    }

    val zero = Stub("")
  }

  def apply(text: String): Int =
    Monoid.foldMapV(text, monoid)(apply) match {
      case Stub(s) => count(s)
      case Part(lStub, words, rStub) => count(lStub) + words + count(rStub)
    }

  def par(text: String): Par[Int] =
    Par.map(Monoid.parFoldMapV(text, monoid)(apply)) {
      case Stub(s) => count(s)
      case Part(lStub, words, rStub) => count(lStub) + words + count(rStub)
    }

  def count(s: String) = s.length min 1

  def apply(c: Char): WC =
    if (c.isLetterOrDigit) Stub(c.toString)
    else Part("", 0, "")
}
