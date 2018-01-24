def isSorted[A](as: Array[A], ordered: (A, A) ⇒ Boolean): Boolean = {
  def go(i: Int, acc: Boolean): Boolean = {
    if ((i < as.length) && (i + 1 < as.length)) go(i + 1, acc && ordered(as(i), as(i + 1)))
    else acc
  }

  go(0, acc = true)
}

isSorted(Array(0), (i1: Int, i2: Int) ⇒ i1 <= i2)
isSorted(Array(0, 0), (i1: Int, i2: Int) ⇒ i1 <= i2)
isSorted(Array(1, 0), (i1: Int, i2: Int) ⇒ i1 <= i2)
isSorted(Array(0, 1, 2), (i1: Int, i2: Int) ⇒ i1 <= i2)
isSorted(Array(0, 2, 2), (i1: Int, i2: Int) ⇒ i1 <= i2)
isSorted(Array(3, 2, 2), (i1: Int, i2: Int) ⇒ i1 <= i2)
