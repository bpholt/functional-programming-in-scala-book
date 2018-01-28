package com.planetholt.fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}

class OptionTest extends FlatSpec with Matchers {
  import Option._

  val some: Option[Int] = Some(0)
  val none: Option[Int] = None
  def safeParseInt(s: String): Option[Int] = Try(s.toInt)

  behavior of "Some"

  it should "apply the given function when mapped" in {
    some.map(_ + 1) should be(Some(1))
  }

  it should "apply the given function when flatmapped" in {
    some.flatMap(_ ⇒ Some(1)) should be(Some(1))
    some.flatMap(_ ⇒ None) should be(None)
  }

  it should "return its value when getOrElse" in {
    some.getOrElse(42) should be(0)
  }

  it should "return itself when orElse" in {
    some.orElse(Some(42)) should be(some)
  }

  it should "apply the filtering function" in {
    some.filter(_ == 0) should be(some)
    some.filter(_ == 1) should be(None)
  }

  behavior of "None"

  it should "return None when mapped" in {
    none.map(_ + 1) should be(None)
  }

  it should "return None when flatMapped" in {
    none.map(_ ⇒ Some(1)) should be(None)
  }

  it should "return the default value when getOrElse" in {
    none.getOrElse(0) should be(0)
  }

  it should "return the default value when orElse" in {
    none.orElse(Some(42)) should be(Some(42))
  }

  it should "return none on filter" in {
    none.filter(_ ⇒ true) should be(None)
  }

  behavior of "map2"

  it should "combine two Somes using the giving binary function" in {
    map2(Some(1), Some(2))(_ + _) should be(Some(3))
  }

  it should "return none if either argument is none" in {
    map2[Int, Int, Int](None, Some(0))(_ + _) should be(None)
    map2[Int, Int, Int](Some(0), None)(_ + _) should be(None)
  }

  behavior of "sequence"

  it should "return None if any of the list are None" in {
    sequence(List(Some(0), None, Some(1))) should be(None)
  }

  it should "return the unwrapped values if everything in the list are Some" in {
    sequence(List(Some(0), Some(1))) should be(Some(List(0, 1)))
  }

  behavior of "traverse"

  it should "return none if f ever returns None" in {
    traverse(List("1", "nope"))(safeParseInt) should be(None)
  }

  it should "return Some with list of mapped values if they all mapped successfully" in {
    traverse(List("1", "2"))(safeParseInt) should be(Some(List(1, 2)))
  }
}

class VarianceTest extends FlatSpec with Matchers {
  import Variance._
  behavior of "mean"

  it should "average the values in the sequence" in {
    mean(Seq(1.0, 2.0, 3.0)) should be(Some(2.0))
    mean(Seq.empty) should be(None)
  }

  behavior of "variance"

  it should "do the math" in {
    variance(Seq(1.0, 2.0, 3.0)) should be(Some(2.0 / 3))
  }

  it should "be none for empty sequences" in {
    variance(Seq.empty) should be(None)
  }
}
