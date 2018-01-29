package com.planetholt.fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  import State._
  type Rand[A] = State[RNG, A]

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  val nonNegativeInt: Rand[Int] = State[RNG, Int](rng ⇒ {
    val (i, nextRng) = rng.nextInt

    (if (i < 0) math.abs(i + 1) else i) → nextRng
  })

  def nonNegativeInt_a(rng: RNG): (Int, RNG) = {
    val (i, nextRng) = rng.nextInt

    (if (i < 0) math.abs(i + 1) else i) → nextRng
  }

  def double_a(rng: RNG): (Double, RNG) = {
    val (i, nextRng) = nonNegativeInt_a(rng)

    (i / (Int.MaxValue.toDouble + 1), nextRng)
  }

  def intDouble_a(rng: RNG): ((Int, Double), RNG) = {
    val (i, nextRng) = rng.nextInt
    val (d, finalRng) = double.run(nextRng)

    (i → d) → finalRng
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double.run(rng)
    val (d2, r2) = double.run(r1)
    val (d3, r3) = double.run(r2)

    (d1, d2, d3) → r3
  }

  def ints_a(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 ⇒ (List.empty[Int], rng)
    case _ ⇒
      val (i, nextRng) = rng.nextInt
      val (is, finalRng) = ints_a(count - 1)(nextRng)

      (i +: is, finalRng)
  }

  val int: Rand[Int] = State[RNG, Int](_.nextInt)

  def nonNegativeEven: Rand[Int] = nonNegativeInt.map(i ⇒ i - 1 % 2)

  val double: Rand[Double] = nonNegativeInt.map(_ / (Int.MaxValue.toDouble + 1))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = ra.map2(rb)((_, _))

  val intDouble: Rand[(Int, Double)] = both(int, double)
  val doubleInt: Rand[(Double, Int)] = both(double, int)

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = nonNegativeInt.flatMap { i ⇒
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

}

import State._

case class State[S, +A](run: S ⇒ (A, S)) {
  def map[B](f: A ⇒ B): State[S, B] = flatMap(a ⇒ unit(f(a)))

  def flatMap[B](f: A ⇒ State[S, B]): State[S, B] = State(s ⇒ {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

  def map2[B, C](sb: State[S, B])(f: (A, B) ⇒ C): State[S, C] =
    flatMap(a ⇒ sb.map(b ⇒ f(a, b)))
}

object State {
//  type State[S, +A] = S ⇒ (A, S)

  def unit[S, A](a: A): State[S, A] = State(s ⇒ (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List.empty[A]))((f, acc) ⇒ f.map2(acc)(_ :: _))

}
