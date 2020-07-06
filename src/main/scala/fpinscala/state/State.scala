package fpinscala.state

object State {

  def rollDieWithOffByOne: Int = {
    val rng = new scala.util.Random
    rng.nextInt(6)
  }

}

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  val int: Rand[Int] = _.nextInt

  def nonNegativeInt: Rand[Int] = rng => {
    val (i, r) = rng.nextInt

    if (i < 0) nonNegativeInt(r) else (i, r)
  }

  def double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble: Rand[(Int, Double)] =
    both(int, double)

  def doubleInt: Rand[(Double, Int)] =
  both(double, int)

  def double3: Rand[(Double, Double, Double)] = rng => {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  def ints(count: Int): Rand[List[Int]] =
  sequence(List.fill(count)(int))

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    (a, _)

  def map[A, B](s: Rand[A])
               (f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])
                   (f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)

    (f(a, b), r2)
  }

  def sequence[A](fs: LazyList[Rand[A]]): Rand[LazyList[A]] =
    fs.foldRight(unit(LazyList.empty[A]))(map2(_, _)(_ #:: _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
