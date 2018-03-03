package Chapter05

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  // Exercise 5.1
  "To list" should "return a list with the content of the stream" in {
    Stream(1 + 1, 2 + 1, 3 + 1).toList should be(List(2, 3, 4))
  }

  // Exercise 5.2
//  "Take" should "return the first n elements" in {
//    Stream(1, 2, 3).take(2) should be(Stream(2, 3))
//  }
//
//  "Drop" should "skip the first n elements and return the rest" in {
//    Stream(1 + 1, 2 + 1, 3 + 1).drop(1) should be(Stream(3, 4))
//  }
//
//  "Take while" should "return the elements as long as the condition is true" in {
//    Stream(1, 2, 3).takeWhile(_ < 3) should be(Stream(1, 2))
//  }
}
