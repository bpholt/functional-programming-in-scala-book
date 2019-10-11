package fpinscala.gettingstarted

import scala.annotation.tailrec

object Fibonacci {
  def fib(n: Int): Int = {
//    @tailrec
    def go(n: Int): Int =
      if (n == 0) 0
      else if (n == 1) 1
      else go(n - 1) + go(n - 2)

    go(n)
  }
}
