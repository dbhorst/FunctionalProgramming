package Chapter02

class Sort {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (ordered(as(n - 1), as(n))) loop(n + 1)
      else false
    }

    loop(1)
  }

  def isOrderedInt(first: Int, second: Int): Boolean =
    first < second

}
