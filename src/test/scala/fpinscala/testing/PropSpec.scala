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
}
