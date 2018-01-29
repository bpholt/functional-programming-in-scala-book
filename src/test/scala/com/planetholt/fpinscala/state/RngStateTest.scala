package com.planetholt.fpinscala.state

import com.planetholt.fpinscala.state.RNG._
import org.scalatest.{FlatSpec, Matchers}

class RngStateTest extends FlatSpec with Matchers {

  behavior of "RNG"

  it should "return a pseudo-random value based on the seed" in {
    val rng = Simple(42L)

    val (n1, rng2) = rng.nextInt

    n1 should be(16159453)
    rng2 should be(Simple(1059025964525L))

    val (n2, rng3) = rng2.nextInt

    n2 should be(-1281479697)
    rng3 should be(Simple(197491923327988L))
  }

  behavior of "nonNegativeInt"

  it should "return a non-negative integer" in {
    val rng = Simple(1059025964525L)

    val (n, _) = nonNegativeInt(rng)

    n should be >= 0
  }

  behavior of "double"

  it should "return a double between 0 and 1" in {
    val rng = Simple(42)

    val (d, _) = double(rng)

    d should (be >= 0.0 and be < 1.0)
  }

  "intDouble" should "return an int and a double" in {
    val rng = Simple(42)

    val ((i: Int, d: Double), nextRng) = intDouble(rng)

    i should not be null.asInstanceOf[Int]
    d should (be >= 0.0 and be < 1.0)
    nextRng should be(Simple(197491923327988L))
  }

  "doubleInt" should "return a double and an int" in {
    val rng = Simple(42)

    val ((d: Double, i: Int), nextRng) = doubleInt(rng)

    d should (be >= 0.0 and be < 1.0)
    i should not be null.asInstanceOf[Int]
    nextRng should be(Simple(197491923327988L))
  }

  "double3" should "return 3 doubles" in {
    val ((d1, d2, d3), nextRng) = double3(Simple(42))

    d1 should (be >= 0.0 and be < 1.0)
    d2 should (be >= 0.0 and be < 1.0)
    d3 should (be >= 0.0 and be < 1.0)

    d1 should not be d2
    d2 should not be d3

    nextRng should be(Simple(259172689157871L))
  }

  "ints" should "return a list of n ints" in {
    val (is, nextRng) = ints(3)(Simple(42L))

    is should have length 3
    is.toSet.toList should have length 3

    nextRng should be(Simple(259172689157871L))
  }

  "nonNegativeLessThan" should "return an int between 0 and n" in {
    val (i, rng) = nonNegativeLessThan(42)(Simple(42L))

    i should (be < 42 and be >= 0)
    rng should be(Simple(1059025964525L))
  }
}
