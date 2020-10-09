package fpinscala.testing

import fpinscala.state.SimpleRNG
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PropSpec extends AnyFlatSpec with Matchers {

  private val initialState = SimpleRNG(5L)

  behavior of "listOfN"

  it should "work in the static or dynamic case" in {
    val static = Gen.listOfN(10, Gen.boolean)
    val dynamic = Gen.boolean.listOfN(Gen.unit(10))

    static.sample.run(initialState) should be(dynamic.sample.run(initialState))
  }

  behavior of "Prop &&"

  it should "combine two passing values into a single Passed" in {
    val expected: Prop = Prop.forAll(Gen.unit(true))(identity) && Prop.forAll(Gen.unit(true))(identity)

    expected.run(1, initialState) should be(Passed)
  }

  it should "combine one passing value and one failed value into a Falsified" in {
    val expected: Prop = Prop.forAll(Gen.unit(true))(identity) && Prop.forAll(Gen.unit(false))(identity)

    expected.run(1, initialState) should be(Falsified("false", 1))
  }

  it should "combine one failed value and one passing value into a Falsified" in {
    val expected: Prop = Prop.forAll(Gen.unit(false))(identity) && Prop.forAll(Gen.unit(true))(identity)

    expected.run(1, initialState) should be(Falsified("false", 1))
  }

  it should "combine two failed values into a single Falsified" in {
    val expected: Prop = Prop.forAll(Gen.unit(false))(identity) && Prop.forAll(Gen.unit(false))(identity)
    expected.run(1, initialState) should be(Falsified("false\nfalse", 0))
  }

  /*
   * 48179997485145L will generate true for the first boolean pulled, with
   * 128185544502587L as the next RNG state value. 128185544502587L will generate
   * false, so if the RNG is threaded thru the two Props, then the left-hand side
   * will be Passed and the right-hand side Falsified.
   */
  it should "thread the rng state thru" in {
    val expected: Prop = Prop.forAll(Gen.boolean)(identity) && Prop.forAll(Gen.boolean)(identity)
    expected.run(1, SimpleRNG(48179997485145L)) should be(Falsified("false", 1))
  }
}
