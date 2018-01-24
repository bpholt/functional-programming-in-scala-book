import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil ⇒ 0
    case Cons(x, xs) ⇒ x + sum(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // exercise 3.2
  def tail[A](list: List[A]): List[A] = list match {
    case Cons(_, tail) ⇒ tail
  }

  // exercise 3.3
  def setHead[A](list: List[A], a: A): List[A] = Cons(a, tail(list))

  // exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop(tail(l), n - 1)

  // exercise 3.5
  def dropWhile[A](l: List[A])(pred: A ⇒ Boolean): List[A] = l match {
    case Cons(h, t) if pred(h) ⇒ dropWhile(t)(pred)
    case _ ⇒ l
  }

  // exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, tail@Cons(_, _)) ⇒ Cons(h, init(tail))
    case Cons(_, Nil) ⇒ Nil
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) ⇒ B): B = as match {
    case Nil ⇒ z
    case Cons(x, xs) ⇒ f(x, foldRight(xs, z)(f))
  }

  // exercise 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) ⇒ b + 1)

  // exercise 3.10
  def foldLeftA[A, B](as: List[A], z: B)(f: (B, A) ⇒ B): B = {
    @tailrec
    def go(l: List[A], acc: B): B = l match {
      case Nil ⇒ acc
      case Cons(h, hs) ⇒ go(hs, f(acc, h))
    }

    go(as, z)
  }

  // exercise 3.10
  @tailrec
  def foldLeftB[A, B](as: List[A], z: B)(f: (B, A) ⇒ B): B = as match {
    case Nil ⇒ z
    case Cons(h, hs) ⇒ foldLeftB(hs, f(z, h))(f)
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) ⇒ B): B = foldLeftB(as, z)(f)
}

// exercise 3.1
val z = List(1, 2, 3, 4, 5) match {
  case Cons(x, Cons(2, Cons(4, _))) ⇒ x
  case Nil ⇒ 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) ⇒ x + y
  case Cons(h, t) ⇒ h + List.sum(t)
  case _ ⇒ 101
}

// exercise 3.2
List.tail(List(1, 2, 3))

// exercise 3.3
val value = List.setHead(List(1, 2, 3), 42)
assert(value == List(42, 2, 3))

// exercise 3.4
val dropped = List.drop(List(1, 2, 3), 2)
assert(dropped == List(3))

// exercise 3.5
val droppedWhile = List.dropWhile(List(1, 2, 3))(_ < 3)
assert(dropped == List(3))

// exercise 3.6
val initList = List.init(List(1, 2, 3, 4))
assert(initList == List(1, 2, 3))

// exercise 3.7
object shortCircuit {
  def product2(ns: List[Int]) = List.foldRight(ns, 1) { (a, b) ⇒
    b match {
      case 0 ⇒ 0
      case _ ⇒
        println(s"$a and $b")

        a * b
    }
  }

  product2(List(1, 2, 3, 0, 3, 4, 5, 1))
}

shortCircuit

// exercise 3.8
val refolded = List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
assert(refolded == List(1, 2, 3))

// exercise 3.9
val length = List.length(List(1, 2, 3))
assert(length == 3)

// exercise 3.10
val leftFolded = List.foldLeft(List(1, 2, 3), 0)(_ + _)
assert(leftFolded == List.foldRight(List(1, 2, 3), 0)(_ + _))

// exercise 3.11
object FoldLeft {
  def sum(ints: List[Int]): Int = List.foldLeft(ints, 0)(_ + _)

  def product(ints: List[Int]): Int = List.foldLeft(ints, 1)(_ * _)

  def length[A](l: List[A]): Int = List.foldLeft(l, 0)((i, _) ⇒ i + 1)
}

FoldLeft.sum(List(1, 2, 3))
FoldLeft.product(List(1, 2, 3))
FoldLeft.length(List(1, 2, 3))

// exercise 3.12
object Reverse {
  def apply[A](l: List[A]): List[A] = List.foldLeft[A, List[A]](l, Nil)((l, e) ⇒ Cons(e, l))
}

assert(Reverse(List(1, 2, 3)) == List(3, 2, 1))

List.foldLeft(List(1, 2, 3, 4), "") { (a: String, b: Int) ⇒
  println(s"a = $a, b = $b")

  a + b
}

// exercise 3.13
object FoldsInTermsOfOtherFolds {
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) ⇒ B): B = List.foldRight(Reverse(as), z)((b, a) ⇒ f(a, b))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) ⇒ B): B = List.foldLeft(Reverse(as), z)((a, b) ⇒ f(b, a))
}

assert(FoldsInTermsOfOtherFolds.foldLeft(List(1, 2, 3, 4), "") { (b: String, a: Int) ⇒
  println(s"a = $a, b = $b")

  a + b
} == List.foldLeft(List(1, 2, 3, 4), "") { (b: String, a: Int) ⇒
  println(s"a = $a, b = $b")

  a + b
})

FoldsInTermsOfOtherFolds.foldRight(List(1, 2, 3, 4), "") { (a: Int, b: String) ⇒
  println(s"a = $a, b = $b")

  a + b
}

// exercise 3.14
object Append {
  def apply[A](a1: List[A], a2: List[A]): List[A] = List.foldLeft(Reverse(a1), a2)((l, e) ⇒ Cons(e, l))
}

val appendViaFoldList = Append(List(1, 2, 3, 4), List(5, 6, 7, 8))
assert(appendViaFoldList == List(1, 2, 3, 4, 5, 6, 7, 8))

// exercise 3.15
object Flatten {
  def apply[A](lists: List[List[A]]): List[A] = List.foldLeft[List[A], List[A]](lists, Nil)((b, a) ⇒ Append(b, a))
}

val flattened = Flatten(List(List(1, 2), List(3, 4), List(5, 6)))
assert(flattened == List(1, 2, 3, 4, 5, 6))

// exercise 3.16
object Increment {
  def apply(l: List[Int]): List[Int] = List.foldLeft[Int, List[Int]](Reverse(l), Nil)((is, i) ⇒ Cons(i + 1, is))
}

val incremented = Increment(List(1, 2, 3))
assert(incremented == List(2, 3, 4))

// exercise 3.17
object DoubleToString {
  def apply(l: List[Double]): List[String] = FoldsInTermsOfOtherFolds.foldRight[Double, List[String]](l, Nil)((d, l) ⇒ Cons(d.toString, l))
}

val doubleToString = DoubleToString(List(1.0, 2.0, 3.0))
assert(doubleToString == List("1.0", "2.0", "3.0"))

// exercise 3.18
object Map {
  def apply[A, B](as: List[A])(f: A ⇒ B): List[B] = FoldsInTermsOfOtherFolds.foldRight[A, List[B]](as, Nil)((a, bs) ⇒ Cons(f(a), bs))
}

val mappedDoubleToString = Map(List(1.0, 2.0, 3.0))(_.toString)
assert(mappedDoubleToString == doubleToString)

// exercise 3.19
object Filter {
  def apply[A](as: List[A])(pred: A ⇒ Boolean): List[A] =
    FoldsInTermsOfOtherFolds.foldRight[A, List[A]](as, Nil)((a, bs) ⇒ if (pred(a)) Cons(a, bs) else bs)
}

val filtered = Filter(List(1, 2, 3))(_ % 2 == 0)
assert(filtered == List(2))

// exercise 3.20
object FlatMap {
  def apply[A, B](as: List[A])(f: A ⇒ List[B]): List[B] = Flatten(Map(as)(f))
}

val flatMapped = FlatMap(List(1, 2, 3))(i ⇒ List(i, i))
assert(flatMapped == List(1, 1, 2, 2, 3, 3))

// exercise 3.21
object FilterViaFlatMap {
  def apply[A](as: List[A])(pred: A ⇒ Boolean): List[A] =
    FlatMap(as)(a ⇒ if (pred(a)) List(a) else Nil)
}

val filteredViaFlatMap = Filter(List(1, 2, 3))(_ % 2 == 0)
assert(filteredViaFlatMap == filtered)

// exercise 3.22
object AddCorrespondingElements {
  def apply(l1: List[Int], l2: List[Int]): List[Int] = {
    @tailrec
    def go(a: List[Int], b: List[Int], acc: List[Int]): List[Int] = (a, b) match {
      case (Cons(e1, t1), Cons(e2, t2)) ⇒ go(t1, t2, Cons(e1 + e2, acc))
      case (Nil, Nil) ⇒ acc
    }

    Reverse(go(l1, l2, Nil))
  }
}

val correspondingElementsAdded = AddCorrespondingElements(List(1, 2, 3), List(4, 5, 6))
assert(correspondingElementsAdded == List(5, 7, 9))

// exercise 3.23
object ZipWith {
  def apply[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    @tailrec
    def go(a: List[A], b: List[B], acc: List[C]): List[C] = (a, b) match {
      case (Cons(e1, t1), Cons(e2, t2)) ⇒ go(t1, t2, Cons(f(e1, e2), acc))
      case (Nil, Nil) ⇒ acc
    }

    Reverse(go(l1, l2, Nil))
  }
}

val zippedWithAddition = ZipWith(List(1, 2, 3), List(4, 5, 6))(_ + _)
assert(zippedWithAddition == correspondingElementsAdded)

// exercise 3.24
object HasSubsequence {
  def apply[A](sup: List[A], sub: List[A]): Boolean = {
    def go(sup: List[A], sub: List[A], anyMatchFound: Boolean): Boolean = (sup, sub) match {
      case (Cons(supElem, supTail), Cons(subElem, subTail)) if supElem == subElem ⇒
        // heads match, check sublists
        go(supTail, subTail, anyMatchFound = true)

      case (Cons(_, supTail), Cons(_, _)) if !anyMatchFound ⇒
        // heads don't match, pop an element from sup
        go(supTail, sub, anyMatchFound)

      case (_, Nil) if anyMatchFound ⇒
        // we've found the end of sub, so if we've found a match, we're good
        true

      case _ ⇒ false
    }

    go(sup, sub, anyMatchFound = false)
  }

  val bigList = List(1, 2, 3, 4)
}

assert(HasSubsequence(HasSubsequence.bigList, List(1, 2)))
assert(HasSubsequence(HasSubsequence.bigList, List(2, 3)))
assert(HasSubsequence(HasSubsequence.bigList, List(4)))
assert(!HasSubsequence(HasSubsequence.bigList, List(2, 4)))
