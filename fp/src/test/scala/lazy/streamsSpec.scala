package fp.Lazy

import org.scalatest._

class StreamsSpec extends FlatSpec with Matchers {
  val stream = Stream(1, 2, 3, 4, 5, 6)
  "toList" should "unwind a stream to a list of strict values" in {
    stream.toList should equal(List(1, 2, 3, 4, 5, 6))
  }

  "take" should "take a given number of elements from a stream" in {
    stream.take(3).toList should equal(Stream(1, 2, 3).toList)
    stream.take(1).toList should equal(Stream(1).toList)
    stream.take(0).toList should equal(Stream.empty.toList)
  }

  "drop" should "drop a given number of elements from a stream" in {
    stream.drop(3).toList should equal(Stream(4, 5, 6).toList)
    stream.drop(1).toList should equal(Stream(2, 3, 4, 5, 6).toList)
    stream.drop(0).toList should equal(stream.toList)
  }

  "takeWhile" should "take all elements of a stream that match the given preficate" in {
    stream.takeWhile(_ < 4).toList should equal(Stream(1, 2, 3).toList)
    stream.takeWhile(_ < 40).toList should equal(stream.toList)
  }

  "takeWhile2" should "act the same as takeWhile" in {
    stream.takeWhile2(_ < 4).toList should equal(stream.takeWhile(_ < 4).toList)
    stream.takeWhile2(_ < 40).toList should equal(stream.takeWhile(_ < 40).toList)
  }

  "headOption2" should "act the same as headOption" in {
    stream.headOption2 should equal(stream.headOption)
    Stream.empty.headOption2 should equal(Stream.empty.headOption)
  }
}
