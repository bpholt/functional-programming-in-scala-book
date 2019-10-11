package fpinscala.gettingstarted

import org.scalatest._
import Fibonacci._

class FibonacciSpec extends FlatSpec with Matchers {

  behavior of "fib function"

  it should "return 0 for fib(0)" in {
    fib(0) should be(0)
  }

  it should "return 1 for fib(1)" in {
    fib(1) should be(1)
  }

  it should "return 1 for fib(2)" in {
    fib(2) should be(1)
  }

  it should "return 2 for fib(3)" in {
    fib(3) should be(2)
  }

  it should "return 5 for fib(5)" in {
    fib(5) should be(5)
  }

}
