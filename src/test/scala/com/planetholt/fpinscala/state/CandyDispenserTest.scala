package com.planetholt.fpinscala.state

import org.scalatest.{FlatSpec, Matchers}

class CandyDispenserTest extends FlatSpec with Matchers {

  import CandyDispenser._

  behavior of "CandyDispenser"

  it should "do the example" in {
    val startingState = Machine(locked = true, candies = 5, coins = 10)
    val actions = List.fill(4)(List(Coin, Turn)).flatten

    val ((coins: Int, candy: Int), machine: Machine) =
      simulateMachine(actions).run(startingState)

    coins should be(14)
    candy should be(1)

    machine should be(Machine(locked = true, candies = 1, coins = 14))
  }

  it should "ignore inputs if out of candy" in {
    val startingState = Machine(locked = true, candies = 0, coins = 10)
    val actions = List.fill(4)(List(Coin, Turn)).flatten

    val ((coins: Int, candy: Int), machine: Machine) =
      simulateMachine(actions).run(startingState)

    candy should be(0)
    coins should be(10)

    machine should be(startingState)
  }

  it should "ignore Coin if unlocked" in {
    val startingState = Machine(locked = false, candies = 10, coins = 10)
    val actions = List(Coin)

    val ((coins: Int, candy: Int), machine: Machine) =
      simulateMachine(actions).run(startingState)

    candy should be(10)
    coins should be(10)

    machine should be(startingState)
  }

  it should "ignore Turn if locked" in {
    val startingState = Machine(locked = true, candies = 10, coins = 10)
    val actions = List(Turn)

    val ((coins: Int, candy: Int), machine: Machine) =
      simulateMachine(actions).run(startingState)

    candy should be(10)
    coins should be(10)

    machine should be(startingState)
  }
}
