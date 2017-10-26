package Chapter02

class Fibonacci {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, last: Int, result: Int): Int =
      if (n <= 0) last
      else go(n - 1, result, last + result)

    go(n, 0, 1)
  }

}
