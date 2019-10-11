package fpinscala.gettingstarted

import scala.annotation.tailrec

object Fibonacci {
  def fib(n: Int): Int = {
    @tailrec
    def go(i: Int, acc1: Int, acc2: Int): Int = i match {
      case 0 => acc1
      case 1 => acc2
      case _ => go(i - 1, acc2, acc1 + acc2)
    }

    go(n, 0, 1)
  }
}
