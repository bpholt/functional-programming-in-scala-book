package fpinscala.laziness

import fpinscala.laziness.Stream.cons
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StreamSpec extends AnyFlatSpec with Matchers {

  behavior of "takeWhile"

  def failInt: Int =
    throw new RuntimeException("This should not have been evaluated")

  it should "take things until the predicate fails" in {
//    Stream(1, 2, 1).takeWhile(i => i % 2 == 1).toList should be(List(1))
    cons(1, cons(2, cons(failInt, Stream.empty[Int]))).takeWhile(i => i % 2 == 1).toList should be(List(1))
  }

  behavior of "forAll"

  it should "return true if the predicate is true for all elements" in {
    Stream(1,2).forAll(_ < 3) should be(true)
  }

  it should "return false if the predicate is false for any elements" in {
    Stream(1,2).forAll(_ > 3) should be(false)
  }

  it should "return false for Ryan's test" in {
    Stream(1,2).forAll(_ < 2) should be(false)
  }
}
