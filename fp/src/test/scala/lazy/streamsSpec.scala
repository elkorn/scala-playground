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

  "append" should "append a stream to a stream" in {
    stream.append(Stream.cons(42, Stream.empty)).toList should equal(Stream(1, 2, 3, 4, 5, 6, 42).toList)
  }

  "appendValue" should "append an element to a stream" in {
    stream.appendValue(42).toList should equal(Stream(1, 2, 3, 4, 5, 6, 42).toList)
  }

  "map" should "apply a mapping fn to all elements" in {
    stream.map(_ + 2).toList should equal(Stream(3, 4, 5, 6, 7, 8).toList)
  }

  "flatMap" should "apply a mapping fn to all elements and flatten the result" in {
    stream.flatMap((x) => Stream(x, x + 1)).toList should equal(Stream(1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7).toList)
  }

  "filter" should "take only the items that match the given predicate" in {
    stream.filter(_ % 2 == 0).toList should equal(Stream(2, 4, 6).toList)
  }

  "find" should "take only the first item that matches the given predicate" in {
    stream.find(_ % 2 == 0) should equal(Some(2))
    stream.find(_ == 9) should equal(None)
  }

  "constant" should "return an infinite stream of given value instances" in {
    Stream.constant(42).take(2).toList should equal(Stream(42, 42).toList)
    Stream.constant(42).take(4).toList should equal(Stream(42, 42, 42, 42).toList)
  }

  "from" should "generate an infinite stream of growing integers, starting from the given value" in {
    Stream.from(1).take(5).toList should equal(Stream(1, 2, 3, 4, 5).toList)
    Stream.from(2).take(7).toList should equal(Stream(2, 3, 4, 5, 6, 7, 8).toList)
  }

  "fibs" should "generate a stream of Fibonacci numbers" in {
    Stream.fibs.take(3).toList should equal(Stream(0, 1, 1).toList)
    Stream.fibs.take(6).toList should equal(Stream(0, 1, 1, 2, 3, 5).toList)
  }

  "unfold" should "use a custom fn to generate stream elements" in {
    Stream.unfold(2)(x => Some(x, x + 2)).take(3).toList should equal(Stream(2, 4, 6).toList)
  }
}
