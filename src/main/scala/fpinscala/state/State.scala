package fpinscala.state

case class State[S, +A](run: S => (A, S)) {

}

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  val int: Rand[Int] = State(_.nextInt)

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  def nonNegativeInt: Rand[Int] = State { rng =>
    val (i, r) = rng.nextInt

    if (i < 0) nonNegativeInt.run(r) else (i, r)
  }

  def double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble: Rand[(Int, Double)] =
    both(int, double)

  def doubleInt: Rand[(Double, Int)] =
  both(double, int)

  def double3: Rand[(Double, Double, Double)] = State { rng =>
    val (d1, r1) = double.run(rng)
    val (d2, r2) = double.run(r1)
    val (d3, r3) = double.run(r2)

    ((d1, d2, d3), r3)
  }

  def ints(count: Int): Rand[List[Int]] =
  sequence(List.fill(count)(int))

  type Rand[+A] = State[RNG, A]

  def unit[A](a: A): Rand[A] =
    State((a, _))

  def map[A, B](s: Rand[A])
               (f: A => B): Rand[B] =
//    rng => {
//      val (a, rng2) = s(rng)
//      (f(a), rng2)
//    }
    flatMap(s)(a => unit(f(a)))

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])
                   (f: (A, B) => C): Rand[C] =
//    rng => {
//      val (a, r1) = ra(rng)
//      val (b, r2) = rb(r1)
//
//      (f(a, b), r2)
//    }
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def sequence[A](fs: LazyList[Rand[A]]): Rand[LazyList[A]] =
    fs.foldRight(unit(LazyList.empty[A]))(map2(_, _)(_ #:: _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))

  def nonNegativeLessThan(n: Int): Rand[Int] =
//    rng => {
//      val (i, rng2) = nonNegativeInt(rng)
//      val mod = i % n
//      if (i + (n - 1) - mod >= 0) (mod, rng2)
//      else nonNegativeLessThan(n)(rng2)
//    }
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = State { rng =>
    val (a, r) = f.run(rng)
    g(a).run(r)
  }

}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
