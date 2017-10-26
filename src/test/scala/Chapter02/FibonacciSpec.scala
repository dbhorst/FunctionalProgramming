package Chapter02

import org.scalatest.{FlatSpec, Matchers}

class FibonacciSpec extends FlatSpec with Matchers {

  "Fibonacci" should "return the last fibonacci number" in {
    val fibonacci = new Fibonacci

    fibonacci.fib(0) should be(0)
    fibonacci.fib(1) should be(1)
    fibonacci.fib(2) should be(1)
    fibonacci.fib(3) should be(2)
    fibonacci.fib(4) should be(3)
    fibonacci.fib(5) should be(5)
    fibonacci.fib(6) should be(8)
  }
}
