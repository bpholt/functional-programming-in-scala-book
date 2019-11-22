package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.scalatest._

class ListSpec extends FlatSpec with Matchers {

  behavior of "stdlib foldLeft"

  it should "build the list in reverse order" in {
    (0 to 10).foldLeft(scala.collection.immutable.List.empty[Int])((b, a) => a :: b) should be((0 to 10).toList.reverse)
  }

  behavior of "stdlib foldRight"

  it should "build the list in order" in {
    (0 to 10).foldRight(scala.collection.immutable.List.empty[Int])(_ :: _) should be((0 to 10).toList)
  }

  behavior of "pattern matching"

  it should "equal 3" in {
    val exercise31 = fpinscala.datastructures.List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    exercise31 should be(3)
  }

  behavior of "tail"

  it should "return the tail of a non-empty list" in {
    val t = tail(List(1, 2, 3))

    t should be(List(2, 3))
  }

  it should "return Nil if the list is empty" in {
    val t = tail(Nil)

    t should be(Nil)
  }

  behavior of "setHead"

  it should "set the head of a non-empty list" in {
    val l = setHead(List(1, 2, 3), 42)

    l should be (List(42, 2, 3))
  }

  it should "set the head of an empty list" in {
    val l = setHead(Nil, 42)

    l should be (List(42))
  }

  behavior of "drop"

  it should "drop the leading elements of the list" in {
    val l = drop(List(1, 2, 3), 2)

    l should be(List(3))
  }

  it should "return Nil if it runs out of elements" in {
    val l = drop(List(1, 2, 3), 3)

    l should be(Nil)
    drop(l, 1) should be(Nil)
  }

  behavior of "dropWhile"

  it should "drop the elements of the list as long as the predicate is true" in {
    val l = dropWhile(List(1, 2, 3))(_ < 3)

    l should be(List(3))
  }

  it should "return Nil if it runs out of elements" in {
    val l = dropWhile(List(1, 2, 3))(_ => true)

    l should be(Nil)
  }

  behavior of "init"

  it should "drop the last element in the list" in {
    val l = init(List(1, 2, 3))

    l should be(List(1, 2))
  }

  behavior of "shortcircuitingFoldRight"

  it should "fold to 0" in {
    shortcircuitingFoldRight(List(0, 1), 1.0)(_ == 0)(_ * _) should be(0.0)
    shortcircuitingFoldRight(List(1, 0), 1.0)(_ == 0)(_ * _) should be(0.0)
  }

  private def listOfLength(i: Int): List[Int] = (0 until i).foldRight(nil[Int])(Cons(_, _))

  behavior of "foldRight"

  it should "fold a list using +" in {
    foldRight(listOfLength(10), 0)(_ + _) should be(45)
  }

  it should "build a list when given Nil and Cons" in {
    foldRight(List(1, 2, 3), nil[Int])(Cons(_, _)) should be(List(1, 2, 3))
  }

  it should "blow up on large lists" in {
    a[StackOverflowError] should be thrownBy foldRight(listOfLength(10000), 0)(_ + _)
  }

  behavior of "foldLeft"

  it should "fold a list using +" in {
    foldLeft(listOfLength(10), 0)(_ + _) should be(45)
  }

  it should "build a list when given Nil and Cons" in {
    foldLeft(List(1, 2, 3), nil[Int])((b, a) => Cons(a, b)) should be(List(3, 2, 1))
  }

  it should "not blow up on large lists" in {
    foldLeft(listOfLength(10000), 0)(_ + _) should be(49995000)
  }

  behavior of "length"

  it should "return the length of the list" in {
    len(List(1, 2, 3)) should be(3)
    len(nil[String]) should be (0)
  }

  behavior of "sumLeft"

  it should "add the numbers" in {
    sumLeft(List(1, 2, 3)) should be(6)
  }

  behavior of "productLeft"

  it should "multiply the numbers" in {
    productLeft(List(1, 2, 3)) should be(6)
  }

  "lenLeft" should "compute the length of the list" in {
    lenLeft(Nil) should be(0)
    lenLeft(listOfLength(0)) should be(0)
    lenLeft(listOfLength(42042)) should be(42042)
  }

  behavior of "reverse"

  it should "reverse the list" in {
    reverse(List(1, 2, 3)) should be(List(3, 2, 1))
  }

  behavior of "foldRightViaFoldLeft"

  it should "fold a list using +" in {
    foldRightViaFoldLeft(listOfLength(10), 0)(_ + _) should be(45)
  }

  it should "build a list when given Nil and Cons" in {
    foldRightViaFoldLeft(List(1, 2, 3), nil[Int])(Cons(_, _)) should be(List(1, 2, 3))
  }

  it should "not blow up on large lists" in {
    foldRightViaFoldLeft(listOfLength(10000), 0)(_ + _) should be(49995000)
  }

  behavior of "foldLeftViaFoldRight"

  it should "fold a list using +" in {
    foldLeftViaFoldRight(listOfLength(10), 0)(_ + _) should be(45)
  }

  it should "build a list when given Nil and Cons" in {
    foldLeftViaFoldRight(List(1, 2, 3), nil[Int])((b, a) => Cons(a, b)) should be(List(3, 2, 1))
  }

  it should "not blow up on large lists" in {
    a[StackOverflowError] should be thrownBy foldLeftViaFoldRight(listOfLength(10000), 0)(_ + _)
  }

}
