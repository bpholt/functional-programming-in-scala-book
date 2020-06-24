package fpinscala.state

import scala.annotation.tailrec

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
  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt

    if (i < 0) nonNegativeInt(r) else (i, r)
  }

  @tailrec
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)

    if (i == Int.MaxValue) double(r) else (i.toDouble / Int.MaxValue, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = RNG.double(r)

    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r) = RNG.double(rng)
    val (i, r2) = r.nextInt

    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = RNG.double(rng)
    val (d2, r2) = RNG.double(r1)
    val (d3, r3) = RNG.double(r2)

    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (l, ro) = List.unfold((rng, count)) {
      case (_, c) if c <= 0 => None
      case (r, c) =>
        val (i, rn) = r.nextInt

        Option(((i, rn), (rn, c - 1)))
    }.unzip

    (l, ro.last)
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
