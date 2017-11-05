package Chapter03

import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers {

  // Exercise 3.1
  "Matcher" should "return the correct match" in {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    x should be(3)
  }

  "Sum" should "sum up all items in a List" in {
    List.sum(List(1, 2, 3, 4)) should be(10)
    List.sum(List()) should be(0)
    List.sum(List(1)) should be(1)
  }

  "Product" should "multiply all items in a List" in {
    List.product(List(1, 2, 3, 4)) should be(24)
    List.product(List(1, 2, 0, 4)) should be(0)
    List.product(List(1.5, 4.5)) should be(6.75)
    List.product(List()) should be(1)
    List.product(List(1)) should be(1)
  }

  // Exercise 3.2
  "Tail" should "remove the first item and return the rest" in {
    List.tail(List(1, 2, 3, 4)) should be(List(2, 3, 4))
    List.tail(List(1)) should be(Nil)
    List.tail(List()) should be(Nil)
  }

  // Exercise 3.3
  "Set head" should "replace the first value with a given value" in {
    List.setHead(List(1, 2, 3, 4), 5) should be(List(5, 2, 3, 4))
    List.setHead(List(1), 5) should be(List(5))
    List.setHead(List(), 5) should be(List(5))
  }

  // Exercise 3.4
  "Drop" should "remove the first n elements" in {
    List.drop(List(1, 2, 3, 4), 2) should be(List(3, 4))
    List.drop(List(1, 2, 3, 4), 0) should be(List(1, 2, 3, 4))
    List.drop(List(1), 1) should be(List())
    List.drop(List(), 1) should be(List())
  }

  // Exercise 3.5
  "Drop while" should "remove elements from the List prefix as long as they match a predicate" in {
    List.dropWhile(List(1, 2, 3, 4), (a: Int) => a < 3) should be(List(3, 4))
    List.dropWhile(List(1, 2, 3, 4), (a: Int) => a > 3) should be(List(1, 2, 3, 4))
    List.dropWhile(List(), (a: Int) => a < 3) should be(List())
  }

  "Append" should "add all the elements of one list to the end of another" in {
    List.append(List(1, 2, 3, 4), List(5, 6, 7, 8)) should be(List(1, 2, 3, 4, 5, 6, 7, 8))
    List.append(List(), List(5, 6, 7, 8)) should be(List(5, 6, 7, 8))
    List.append(List(1, 2, 3, 4), List()) should be(List(1, 2, 3, 4))
    List.append(Nil, List(5, 6, 7, 8)) should be(List(5, 6, 7, 8))
    List.append(List(1, 2, 3, 4), Nil) should be(List(1, 2, 3, 4))
  }

  "Init" should "return a List consisting of all but the last element of a List" in {
    List.init(List(1, 2, 3, 4, 5)) should be(List(1, 2, 3, 4))
    List.init(List(1)) should be(List())
    List.init(List()) should be(List())
    List.init(Nil) should be(Nil)
  }
}
