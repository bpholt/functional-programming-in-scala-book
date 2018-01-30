package com.planetholt.fpinscala.state

import State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyDispenser_a {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State[Machine, (Int, Int)] { machine =>
      val finalState = inputs.foldLeft(machine) { (machine, input) ⇒
        input match {
          case _ if machine.candies == 0 ⇒ machine
          case Coin if machine.locked ⇒ machine.copy(locked = false, coins = machine.coins + 1)
          case Turn if !machine.locked ⇒ machine.copy(locked = true, candies = machine.candies - 1)
          case _ ⇒ machine
        }
      }

      ((finalState.coins, finalState.candies), finalState)
    }
}

object CandyDispenser {
  private def update(i: Input): Machine ⇒ Machine = s ⇒ {
    (i: Input, s: Machine) match {
      case (_, Machine(_, 0, _)) ⇒ s
      case (Coin, Machine(true, candies, coins)) ⇒ Machine(locked = false, candies, coins + 1)
      case (Turn, Machine(false, candies, coins)) ⇒ Machine(locked = true, candies - 1, coins)
      case _ ⇒ s
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ ← sequence(inputs.map(update _ andThen modify[Machine]))
    s ← get
  } yield (s.coins, s.candies)
}
