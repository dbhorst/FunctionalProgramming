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

    List.sum2(List(1, 2, 3, 4)) should be(10)
    List.sum2(List()) should be(0)
    List.sum2(List(1)) should be(1)

    List.sum3(List(1, 2, 3, 4)) should be(10)
    List.sum3(List()) should be(0)
    List.sum3(List(1)) should be(1)
  }

  "Product" should "multiply all items in a List" in {
    List.product(List(1, 2, 3, 4)) should be(24)
    List.product(List(1, 2, 0, 4)) should be(0)
    List.product(List(1.5, 4.5)) should be(6.75)
    List.product(List()) should be(1)
    List.product(List(1)) should be(1)

    List.product2(List(1, 2, 3, 4)) should be(24)
    List.product2(List(1, 2, 0, 4)) should be(0)
    List.product2(List(1.5, 4.5)) should be(6.75)
    List.product2(List()) should be(1)
    List.product2(List(1)) should be(1)

    List.product3(List(1, 2, 3, 4)) should be(24)
    List.product3(List(1, 2, 0, 4)) should be(0)
    List.product3(List(1.5, 4.5)) should be(6.75)
    List.product3(List()) should be(1)
    List.product3(List(1)) should be(1)
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

    List.dropWhileCurry(List(1, 2, 3, 4, 5))(x => x < 4) should be(List(4, 5))
  }

  "Append" should "add all the elements of one list to the end of another" in {
    List.append(List(1, 2, 3, 4), List(5, 6, 7, 8)) should be(List(1, 2, 3, 4, 5, 6, 7, 8))
    List.append(List(), List(5, 6, 7, 8)) should be(List(5, 6, 7, 8))
    List.append(List(1, 2, 3, 4), List()) should be(List(1, 2, 3, 4))
    List.append(Nil, List(5, 6, 7, 8)) should be(List(5, 6, 7, 8))
    List.append(List(1, 2, 3, 4), Nil) should be(List(1, 2, 3, 4))

    List.append2(List(1, 2, 3, 4), List(5, 6, 7, 8)) should be(List(1, 2, 3, 4, 5, 6, 7, 8))
    List.append2(List(), List(5, 6, 7, 8)) should be(List(5, 6, 7, 8))
    List.append2(List(1, 2, 3, 4), List()) should be(List(1, 2, 3, 4))
    List.append2(Nil, List(5, 6, 7, 8)) should be(List(5, 6, 7, 8))
    List.append2(List(1, 2, 3, 4), Nil) should be(List(1, 2, 3, 4))
  }

  // Exercise 3.6
  "Init" should "return a List consisting of all but the last element of a List" in {
    List.init(List(1, 2, 3, 4, 5)) should be(List(1, 2, 3, 4))
    List.init(List(1)) should be(List())
    List.init(List()) should be(List())
    List.init(Nil) should be(Nil)
  }

  // Exercise 3.8
  "Passing Nil and Cons" should "work with foldRight" in {
    List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) should be(List(1, 2, 3))
  }

  // Exercise 3.9
  "Length" should "compute the length of a List" in {
    List.length(List(1, 2, 3, 4, 5)) should be(5)
    List.length(List()) should be(0)

    List.length2(List(1, 2, 3, 4, 5)) should be(5)
    List.length2(List()) should be(0)
  }

  // Exercise 3.12
  "Reverse" should "reverse a given List" in {
    List.reverse(List(1, 2, 3, 4, 5)) should be(List(5, 4, 3, 2, 1))
    List.reverse(List(1.1, 2.2, 3.3)) should be(List(3.3, 2.2, 1.1))
    List.reverse(List()) should be(List())
  }

  // Exercise 3.15
  "Concat" should "concatinate a List of Lists into a single List" in {
    var ass = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    List.concat(ass) should be(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    List.concat(List(List(1, 2, 3))) should be(List(1, 2, 3))
    List.concat(List()) should be(List())
  }

  // Exercise 3.16
  "Transform" should "add 1 to every element of a List" in {
    List.transform(List(1, 2, 3)) should be(List(2, 3, 4))
    List.transform(List()) should be(List())
    List.transform(Nil) should be(Nil)

    List.transform2(List(1, 2, 3)) should be(List(2, 3, 4))
    List.transform2(List()) should be(List())
    List.transform2(Nil) should be(Nil)
  }

  // Exercise 3.17
  "Double to String" should "convert every Double of a List to String" in {
    List.doubleToString(List(1.1, 2.2, 3.3)) should be(List("1.1", "2.2", "3.3"))
    List.doubleToString(List()) should be(List())
    List.doubleToString(Nil) should be(Nil)
  }

  // Exercise 3.18
  "Map" should "modify every element in the given List" in {
    List.map(List(1, 2, 3))(_ + 1) should be(List(2, 3, 4))
    List.map(List(1, 2, 3))(_ * 3) should be(List(3, 6, 9))
    List.map(List(): List[Int])(_ + 1) should be(List())
  }

  // Exercise 3.19
  "Filter" should "filter the elements in the given List" in {
    List.filter(List(1, 2, 3, 4, 5, 6))((x) => x % 2 == 1) should be(List(2, 4, 6))
    List.filter(List(1, 2, 3, 4, 5, 6))((x) => x % 2 == 0) should be(List(1, 3, 5))
    List.filter(List(): List[Int])((x) => x % 2 == 0) should be(List())

    // Exercise 3.21
    List.filter2(List(1, 2, 3, 4, 5, 6))((x) => x % 2 == 1) should be(List(2, 4, 6))
    List.filter2(List(1, 2, 3, 4, 5, 6))((x) => x % 2 == 0) should be(List(1, 3, 5))
    List.filter2(List(): List[Int])((x) => x % 2 == 0) should be(List())
  }

  // Exercise 3.20
  "Flat map" should "modify every elements in the given List to a List" in {
    List.flatMap(List(1, 2, 3))(i => List(i, i)) should be(List(1, 1, 2, 2, 3, 3))
    List.flatMap(List())(i => List(i, i)) should be(List())
  }

  // Exercise 3.22
  "Add list" should "add every element of the fist and second list" in {
    List.addLists(List(1, 2, 3), List(4, 5, 6)) should be(List(5, 7, 9))
    List.addLists(List(), List()) should be(List())
  }

  // Exercise 3.23
  "Zip with" should "perform a function on every element of the fist and the second list" in {
    List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) should be(List(5, 7, 9))
    List.zipWith(List():List[Int], List(): List[Int])(_ + _) should be(List())

    List.zipWith(List(2.0, 4.0, 6.0), List(0.5, 0.5, 0.5))(_ * _) should be(List(1.0, 2.0, 3.0))
    List.zipWith(List():List[Double], List(): List[Double])(_ * _) should be(List())
  }

}
