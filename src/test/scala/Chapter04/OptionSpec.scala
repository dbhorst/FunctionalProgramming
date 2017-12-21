package Chapter04

import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {

  // Exercise 4.1
  "Map" should "modify the Element in the Option" in {
    Some(1).map(_ + 1) should be(Some(2))
    Some(List(1, 2, 3)).map(_.map(_+1)) should be(Some(List(2, 3, 4)))
    None.map(x => x) should be(None)
  }

  "FlatMap" should "modify the Element in the Option and return a new Option" in {
    Some(1).flatMap(i => Some(List(i, i))) should be(Some(List(1, 1)))
    None.flatMap(x => x) should be(None)
  }

  "GetOrElse" should "return the result or (if there is none) a default value" in {
    Some(1).getOrElse(2) should be(1)
    None.getOrElse(2) should be(2)
  }

  "OrElse" should "return a default value if the object is None" in {
    Some(1).orElse(Some(2)) should be(Some(1))
    None.orElse(Some(2)) should be(Some(2))
  }

  "Filter" should "filter the Option" in {
    Some(1).filter(_ > 2) should be(None)
    Some(4).filter(_ > 2) should be(Some(4))
    None.filter(_ => true) should be(None)
    None.filter(_ => false) should be(None)
  }

  // Exercise 4.2
  "Variance" should "return the variance of a list" in {
    val list = List(8.0, 10.0, 6.0, 7.0, 9.0)
    Option.variance(list) should be(Some(2.0))
    Option.variance(List()) should be(None)
  }

  // Exercise 4.3
  "Map2" should "map two Options on a function without Option" in {
    Option.map2(Some(1), Some(2))(math.max) should be(Some(2))
    Option.map2(None, Some(2))(math.max) should be(None)
    Option.map2(Some(1), None)(math.max) should be(None)
    Option.map2(None, None)(math.max) should be(None)
  }

  // Exercise 4.4
  "Sequence" should "combine a list of Options into one Option containing a list of all the values" in {
    Option.sequence(List(Some(1), Some(2), Some(3))) should be(Some(List(1, 2, 3)))
    Option.sequence(List(Some(1), None, Some(3))) should be(None)
    Option.sequence(List(None)) should be(None)
  }
}
