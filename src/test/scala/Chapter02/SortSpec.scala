package Chapter02

import org.scalatest.{FlatSpec, Matchers}

class SortSpec extends FlatSpec with Matchers {

  "Ordered Test" should "true if the array is ordered" in {
    val sort = new Sort

    sort.isOrderedInt(1, 2) should be(true)
    sort.isOrderedInt(2, 3) should be(true)
    sort.isOrderedInt(1, 5) should be(true)
    sort.isOrderedInt(2, 2) should be(false)
    sort.isOrderedInt(5, 2) should be(false)
  }

  "Sort Test" should "true if the array is sorted" in {
    val sort = new Sort

    sort.isSorted(Array(1, 2, 3, 4), sort.isOrderedInt) should be(true)
    sort.isSorted(Array(1, 2, 7, 4), sort.isOrderedInt) should be(false)

    sort.isSorted(Array(1.1, 1.2, 2.3, 5.0), (x: Double, y: Double) => x < y) should be(true)
    sort.isSorted(Array(1.1, 1.4, 1.3, 5.0), (x: Double, y: Double) => x < y) should be(false)
  }

}
