package com.planetholt.fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}

class EitherTest extends FlatSpec with Matchers {
  import Either._

  behavior of "Left"

  it should "return itself on map" in {
    Left(TestException).map(_ ⇒ 1) should be(Left(TestException))
  }
  it should "return itself on flatMap" in {
    Left(TestException).flatMap(_ ⇒ Right(1)) should be(Left(TestException))
  }
  it should "return the alternative on orElse" in {
    Left(TestException).orElse(Right(1)) should be(Right(1))
  }
  it should "return itself on map2" in {
    val left: Either[Exception, Int] = Left(TestException)
    left.map2(Right(2))(_ + _) should be(left)
    left.map2(Left(new RuntimeException("different test exception", null, true, false) {}))(_ + _) should be(left)
  }

  behavior of "Right"

  it should "return the mapped value on map" in {
    Right(0).map(_ + 1) should be(Right(1))
  }

  it should "return the flatMapped value" in {
    val value: Either[Exception, Int] = Right(0).flatMap(_ ⇒ Right(1))
    value should be(Right(1))
    Right(0).flatMap(_ ⇒ Left(TestException)) should be(Left(TestException))
  }

  it should "return itself on orElse" in {
    Right(0).orElse(Right(1)) should be(Right(0))
  }

  it should "apply the function to the given either, if it too is a Right" in {
    Right(1).map2(Right(2))(_ + _) should be(Right(3))
  }

  it should "return the other either if it is a Left" in {
    Right(1).map2(Left(TestException))(_ + _) should be(Left(TestException))
  }

  behavior of "traverse"

  case class StringToIntException(msg: String) extends RuntimeException(msg, null, true, false)
  def safeToInt(s: String): Either[StringToIntException, Int] = try Right(s.toInt) catch {
    case e: NumberFormatException ⇒ Left(StringToIntException(e.getMessage))
  }

  it should "return the first Left value encountered" in {
    traverse(List("1", "nope", "3", "nope2"))(safeToInt) should be(Left(StringToIntException("""For input string: "nope"""")))
  }

  it should "return the list of mapped values if all succeed" in {
    traverse(List("1", "2", "3"))(s => Try(s.toInt)) should be(Right(List(1, 2, 3)))
  }

  behavior of "sequence"

  it should "return the first Left value encountered" in {
    sequence(List(Right(0), Left(StringToIntException("nope1")), Left(StringToIntException("nope2")))) should be(Left(StringToIntException("nope1")))
  }

  it should "return the list of values if all are Right" in {
    sequence(List(Right(0), Right(1))) should be(Right(List(0, 1)))
  }
}

object TestException extends RuntimeException("test exception", null, true, false)
