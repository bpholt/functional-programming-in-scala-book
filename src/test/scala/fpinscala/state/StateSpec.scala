package fpinscala.state

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateSpec extends AnyFlatSpec with Matchers {
  val rng: RNG = SimpleRNG(42L)

  val firstInt: Int = 16159453
  val secondInt: Int = -1281479697
  val thirdInt: Int = -340305902
  val fourthInt: Int = -2015756020
  val fifthInt: Int = 1770001318
  val firstDouble: Double = 0.007524831689672932
  val secondDouble: Double = 0.824221092660083
  val thirdDouble: Double = 0.47307205967282506

  behavior of "SimpleRNG"

  it should "return values based on the seed" in {
    val (n1, rng2) = rng.nextInt

    val (n2, _) = rng2.nextInt

    n1 should be (firstInt)
    n2 should be (secondInt)
  }

  behavior of "nonNegativeInt"

  it should "return a non-negative integer" in {
    val (_, rng) = SimpleRNG(42L).nextInt

    val (output, _) = RNG.nonNegativeInt(rng)

    output should be (fifthInt)
  }

  it should "handle the Int.MinValue corner case" in {
    val (output, _) = RNG.nonNegativeInt(new RNG {
      override def nextInt: (Int, RNG) = (Int.MinValue, SimpleRNG(42L))
    })

    output should be (firstInt)
  }

  behavior of "double"

  it should "return a double between 0 and 1" in{
    val (output, _) = RNG.double(rng)

    output should be(firstDouble)
  }

  it should "handle the Int.MaxValue corner case" in {
    val (output, _) = RNG.double(new RNG {
      override def nextInt: (Int, RNG) = (Int.MaxValue, SimpleRNG(42L))
    })

    output should be(firstDouble)
  }

  behavior of "intDouble"

  it should "return an int and a double" in {
    val ((i, d), _) = RNG.intDouble(rng)

    i should be(firstInt)
    d should be(secondDouble)
  }

  behavior of "doubleInt"

  it should "return an int and a double" in {
    val ((d, i), _) = RNG.doubleInt(rng)

    d should be(firstDouble)
    i should be(secondInt)
  }

  behavior of "double3"

  it should "return 3 doubles" in {
    val ((d1, d2, d3), _) = RNG.double3(rng)

    d1 should be(firstDouble)
    d2 should be(secondDouble)
    d3 should be(thirdDouble)
  }

  behavior of "ints"

  it should "return the given number of ints" in {
    val (output, _) = RNG.ints(3)(rng)

    output should be(List(firstInt, secondInt, thirdInt))
  }

  it should "return an RNG in the correct state" in {
    val (_, output) = RNG.ints(3)(rng)
    val (nextInt, _) = output.nextInt

    nextInt should be(fourthInt)
  }
}
