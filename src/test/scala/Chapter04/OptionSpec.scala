package Chapter04

import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {

  // Execise 4.1
  "Map" should "modify every element" in {
    Some(1).map(_ + 1) should be(Some(2))
    Some(List(1, 2, 3)).map(_.map(_+1)) should be(Some(List(2, 3, 4)))
    None.map(x => x) should be(None)
  }

  "FlatMap" should "modify every elements in the given List to a List" in {
    Some(1).flatMap(i => Some(List(i, i))) should be(Some(List(1, 1)))
    None.map(x => x) should be(None)
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
  
}
