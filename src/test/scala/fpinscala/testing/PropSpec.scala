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

    expected.run(1, initialState) should be(Falsified("false", 0))
  }

  it should "combine one failed value and one passing value into a Falsified" in {
    val expected: Prop = Prop.forAll(Gen.unit(false))(identity) && Prop.forAll(Gen.unit(true))(identity)

    expected.run(1, initialState) should be(Falsified("false", 0))
  }

  it should "combine two failed values into a single Falsified" in {
    val expected: Prop = Prop.forAll(Gen.unit(false))(identity) && Prop.forAll(Gen.unit(false))(identity)
    expected.run(1, initialState) should be(Falsified("false", 0))
  }

  behavior of "Prop ||"

  it should "combine two passing values into a single Passed" in {
    val expected: Prop = Prop.forAll(Gen.unit(true))(identity) || Prop.forAll(Gen.unit(true))(identity)

    expected.run(1, initialState) should be(Passed)
  }

  it should "combine one passing value and one failed value into a Passed" in {
    val expected: Prop = Prop.forAll(Gen.unit(true))(identity) || Prop.forAll(Gen.unit(false))(identity)

    expected.run(1, initialState) should be(Passed)
  }

  it should "combine one failed value and one passing value into a Passed" in {
    val expected: Prop = Prop.forAll(Gen.unit(false))(identity) || Prop.forAll(Gen.unit(true))(identity)

    expected.run(1, initialState) should be(Passed)
  }

  it should "combine two failed values into a single Falsified" in {
    val expected: Prop = Prop.forAll(Gen.unit(false))(identity) || Prop.forAll(Gen.unit(false))(identity)
    expected.run(1, initialState) should be(Falsified("false", 0))
  }

  behavior of "SGen.listOf"

  it should "be the equivalent of Gen.listOfN, given the same list sizes" in {
    val static: Gen[List[Boolean]] = Gen.listOfN(10, Gen.boolean)
    val sGen: SGen[List[Boolean]] = SGen.listOf(Gen.boolean)

    static.sample.run(initialState) should be(sGen.forSize(10).sample.run(initialState))
  }
}
