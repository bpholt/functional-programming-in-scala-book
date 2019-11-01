package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.scalatest._

class ListSpec extends FlatSpec with Matchers {

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

}
