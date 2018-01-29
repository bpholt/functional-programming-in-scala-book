package com.planetholt.fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  import State._
  type Rand[+A] = State[RNG, A]

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

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nextRng) = rng.nextInt

    (if (i < 0) math.abs(i + 1) else i) → nextRng
  }

  def double_a(rng: RNG): (Double, RNG) = {
    val (i, nextRng) = nonNegativeInt(rng)

    (i / (Int.MaxValue.toDouble + 1), nextRng)
  }

  def intDouble_a(rng: RNG): ((Int, Double), RNG) = {
    val (i, nextRng) = rng.nextInt
    val (d, finalRng) = double(nextRng)

    (i → d) → finalRng
  }

  def doubleInt_a(rng: RNG): ((Double, Int), RNG) = {
    val (i, nextRng) = rng.nextInt
    val (d, finalRng) = double(nextRng)

    (d → i) → finalRng
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    (d1, d2, d3) → r3
  }

  def ints_a(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 ⇒ (List.empty[Int], rng)
    case _ ⇒
      val (i, nextRng) = rng.nextInt
      val (is, finalRng) = ints_a(count - 1)(nextRng)

      (i +: is, finalRng)
  }

  val int: Rand[Int] = _.nextInt

//  def unit[A](a: A): Rand[A] = rng ⇒ (a, rng)

  def map_a[A, B](s: Rand[A])(f: A ⇒ B): Rand[B] = rng ⇒ {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

//  def map[A, B](s: Rand[A])(f: A ⇒ B): Rand[B] = flatMap(s)(f andThen unit)

//  def flatMap[A, B](f: Rand[A])(g: A ⇒ Rand[B]): Rand[B] = { rng ⇒
//    val (a, nextRng): (A, RNG) = f(rng)
//
//    g(a)(nextRng)
//  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i ⇒ i - 1 % 2)

  val double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2_a[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] = rng ⇒ {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)

    (f(a, b), rng3)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] =
    flatMap(ra)(a ⇒ map(rb)(b ⇒ f(a, b)))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val intDouble: Rand[(Int, Double)] = both(int, double)
  val doubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit[RNG, List[A]](List.empty[A]))((f, acc) ⇒ map2(f, acc)(_ :: _))

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i ⇒
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

}

//case class State[S, +A](run: S ⇒ (A, S))

object State {
  type State[S, +A] = S ⇒ (A, S)


  def map[S, A, B](s: State[S, A])(f: A ⇒ B): State[S, B] = flatMap(s)(f andThen unit)

  def flatMap[S, A, B](f: State[S, A])(g: A ⇒ State[S, B]): State[S, B] = { s ⇒
    val (a, s1): (A, S) = f(s)
    g(a)(s1)
  }

  def unit[S, A](a: A): State[S, A] = s ⇒ (a, s)
}
