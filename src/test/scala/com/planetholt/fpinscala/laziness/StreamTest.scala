package com.planetholt.fpinscala.laziness

import org.scalatest.{FlatSpec, Matchers}
import Stream._

import scala.collection.mutable
import scala.concurrent.Promise

class StreamTest extends FlatSpec with Matchers {

  def intStreamWithPromisesToProveLaziness(count: Int): (Stream[Int], Seq[Promise[Unit]]) = {
    val promises = (0 until count).map(_ ⇒ Promise[Unit])
    val stream = promises.zipWithIndex.foldRight(Stream.empty[Int])((tuple, b) ⇒ tuple match {
      case (p, i) ⇒ cons({ p.success(()); i }, b)
    })

    (stream, promises)
  }

  behavior of "Stream.toList"

  it should "convert the stream to a list" in {
    Stream(0, 1, 2, 3).toList should be((0 to 3).toList)
  }

  it should "return an empty list from the empty stream" in {
    Stream.empty[Int].toList should be(List.empty[Int])
  }

  behavior of "Stream.take"

  it should "return the first n element of the stream" in {
    Stream(0, 1, 2, 3).take(3).toList should be((0 to 2).toList)
  }

  it should "return the entire stream if n > stream.length" in {
    Stream(0, 1).take(3).toList should be(Stream(0, 1).toList)
  }

  it should "return the empty stream if n is negative" in {
    Stream(0, 1).take(-1).toList should be(Stream.empty[Int].toList)
  }

  it should "only evaluate elements up to the first failed predicate" in {
    val (stream, promises) = intStreamWithPromisesToProveLaziness(2)

    stream.take(1).toList should be(List(0))

    promises.last.isCompleted should be(false)
    promises.head.isCompleted should be(true)
  }

  behavior of "Stream.drop"

  it should "skip the first n elements of the stream" in {
    Stream(0, 1, 2).drop(2).toList should be(List(2))
  }

  it should "return the empty stream if n > stream.length" in {
    Stream(0, 1, 2).drop(4).toList should be(List.empty[Int])
  }

  it should "return the entire stream if n < 0" in {
    Stream(0, 1, 2).drop(-1).toList should be((0 to 2).toList)
  }

  behavior of "Stream.takeWhile"

  it should "return elements that match the given predicate" in {
    Stream(0, 1, 2).takeWhile(_ < 2).toList should be((0 to 1).toList)
  }

  it should "only evaluate elements up to the first failed predicate" in {
    val (stream, promises) = intStreamWithPromisesToProveLaziness(3)

    stream.takeWhile(_ < 1).toList should be(List(0))

    promises(2).isCompleted should be(false)
    promises(1).isCompleted should be(true)
    promises.head.isCompleted should be(true)
  }

  behavior of "Stream.forAll"

  it should "return true if the predicate is true for all values" in {
    Stream(0, 1, 2).forAll(_ < 3) should be(true)
  }

  it should "return false if any predicate is false, and terminate as soon as possible" in {
    val (stream, promises) = intStreamWithPromisesToProveLaziness(2)

    stream.forAll(_ == 2) should be(false)

    promises.head.isCompleted should be(true)
    promises.last.isCompleted should be(false)
  }

  behavior of "Stream.headOption"

  it should "return Some(x) if the stream has elements" in {
    val (stream, promises) = intStreamWithPromisesToProveLaziness(2)

    stream.headOption should be(Option(0))

    promises(1).isCompleted should be(false)
    promises.head.isCompleted should be(true)
  }

  it should "return None if the stream has no elements" in {
    Stream.empty.headOption should be(None)
  }

  behavior of "Stream.map"

  it should "apply the given function to the elements of the stream" in {
    Stream(0, 1).map(_ + 1).toList should be(List(1, 2))
  }

  it should "be lazy" in {
    val (stream, promises) = intStreamWithPromisesToProveLaziness(2)
    stream.map(_ + 1).headOption should be(Option(1))

    promises(1).isCompleted should be(false)
    promises.head.isCompleted should be(true)
  }

  behavior of "Stream.filter"

  it should "remove elements from the stream if they don't match the given predicate" in {
    Stream(0, 1, 2).filter(_ % 2 == 0).toList should be(List(0, 2))
  }

  it should "be lazy" in {
    val (stream, promises) = intStreamWithPromisesToProveLaziness(2)

    stream.filter(_ % 2 == 0).headOption should be(Option(0))

    promises.head.isCompleted should be(true)
    promises.tail.exists(_.isCompleted) should be(false)
  }

  behavior of "find"

  it should "be lazy" in {
    val (stream, promises) = intStreamWithPromisesToProveLaziness(2)

    stream.find(_ == 0) should be(Option(0))

    promises.head.isCompleted should be(true)
    promises.tail.exists(_.isCompleted) should be(false)
  }

  behavior of "Stream.append"

  it should "append the given stream to the end of the stream" in {
    Stream(0, 1).append(Stream(2, 3)).toList should be((0 to 3).toList)
  }

  it should "be lazy" in {
    val (stream1, promises1) = intStreamWithPromisesToProveLaziness(2)
    val (stream2, promises2) = intStreamWithPromisesToProveLaziness(1)

    stream1.append(stream2).take(2).toList should be(List(0, 1))

    promises1.forall(_.isCompleted) should be(true)
    promises2.exists(_.isCompleted) should be(false)
  }

  behavior of "flatMap"

  it should "apply the function and concatenate the resulting streams" in {
    def expandByTen(i: Int): Seq[Int] = (i * 10) to (i * 10 + 9)

    Stream(0, 1).flatMap(i ⇒ Stream(expandByTen(i): _*)).toList should be((0 to 19).toList)
  }

  behavior of "ones"

  it should "return" in {
    ones.take(5).toList should be((0 until 5).map(_ ⇒ 1))
    ones.exists(_ % 2 != 0) should be(true)
    ones.map(_ + 1).exists(_ % 2 == 0) should be(true)
    ones.forAll(_ != 1) should be(false)
  }

  behavior of "constant"

  it should "give a stream of the given value" in {
    constant("a").take(5).toList should be((0 until 5).map(_ ⇒ "a"))
  }

  behavior of "from"

  it should "return an infinite stream of incrementing integers starting from the given value" in {
    from(42).take(5).toList should be((42 until 47).toList)
  }

  "fibs" should "return the fibonacci sequence" in {
    fibs.take(7).toList should be(List(0, 1, 1, 2, 3, 5, 8))
  }

  behavior of "unfold"

  it should "take an initial state and a function for producing the next state and the next value in the stream" in {
    val pages = Map(1 → "one", 2 → "two", 3 → "three")

    unfold(1) { state ⇒
      if (pages.contains(state)) Option(pages(state) → (state + 1))
      else None
    }.toList should be(List("one", "two", "three"))
  }

  behavior of "zipWith"

  it should "zip two streams, applying the given binary function" in {
    Stream(0, 1, 2).zipWith(Stream(3, 4, 5))(_ + _).toList should be(List(3, 5, 7))
  }

  it should "zip two streams, consuming all of stream1 if stream2 is longer" in {
    Stream(0, 1).zipWith(Stream(3, 4, 5))(_ + _).toList should be(List(3, 5))
  }

  it should "zip two streams, consuming all of stream2 if stream1 is longer" in {
    Stream(0, 1, 2).zipWith(Stream(3, 4))(_ + _).toList should be(List(3, 5))
  }

  behavior of "zipAll"

  it should "zip two streams" in {
    Stream(0, 1, 2).zipAll(Stream(3, 4, 5)).toList should be(List(Option(0) → Option(3), Option(1) → Option(4), Option(2) → Option(5)))
  }

  it should "zip two streams even if the first is shorter" in {
    Stream(0, 1).zipAll(Stream(3, 4, 5)).toList should be(List(Option(0) → Option(3), Option(1) → Option(4), None → Option(5)))
  }

  it should "zip two streams even if the second is shorter" in {
    Stream(0, 1, 2).zipAll(Stream(3, 4)).toList should be(List(Option(0) → Option(3), Option(1) → Option(4), Option(2) → None))
  }

  behavior of "startsWith"

  it should "check if the given stream is our prefix" in {
    Stream(1, 2, 3) startsWith Stream(1, 2) should be(true)
    Stream(1, 2, 3) startsWith Stream(2, 3) should be(false)
  }

  it should "return false if the substream is longer than the parent" in {
    Stream(1, 2) startsWith Stream(1, 2, 3) should be(false)
  }

  behavior of "tails"

  it should "return the Stream of suffixes of the input sequence" in {
    Stream(1, 2, 3).tails.map(_.toList).toList should be(List(1, 2, 3).tails.toList)
  }

  behavior of "scanRight"

  it should "foldRight but return a stream of intermediate results" in {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList should be(List(6, 5, 3, 0))
  }

  it should "be lazy" in {
    val calculations = mutable.Buffer.empty[String]
    Stream(1, 2, 3).scanRight(0)((a, b) ⇒ {
      calculations += s"$a -> $b"
      a + b
    }).toList should be(List(1, 2, 3).scanRight(0)(_ + _))

    calculations.toList should be(calculations.toSet.toList)
  }
}
