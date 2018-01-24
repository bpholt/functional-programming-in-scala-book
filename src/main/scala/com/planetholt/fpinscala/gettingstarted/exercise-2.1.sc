def fib(n: Int): Int = {
  @annotation.tailrec
  def go(i: Int, acc: Int, acc2: Int): Int = i match {
    case 0 ⇒ acc
    case 1 ⇒ acc2
    case _ ⇒ go(i - 1, acc2, acc + acc2)
  }
  go(n, 0, 1)
}

fib(0)
fib(1)
fib(2)
fib(3)
fib(4)
fib(5)
