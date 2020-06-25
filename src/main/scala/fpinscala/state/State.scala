package fpinscala.state

import cats.implicits._

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
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt

    if (i < 0) nonNegativeInt(r) else (i, r)
//    if (i < 0) (-i, r) else (i, r)
//    (if(i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)

    if (i == Int.MaxValue) double(r) else (i.toDouble / Int.MaxValue, r)
//    (i / (Int.MaxValue.toDouble + 1), r)

//    if (i == 0) (0.0, r) else {
//      (1.0 / i.toDouble, r)
//    }
  }
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)

    ((i, d), r2)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r) = double(rng)
    val (i, r2) = r.nextInt

    ((d, i), r2)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
//    val (l, r_out) = List.unfold((rng, count)) {
//      case (_, c) if c <= 0 => None
//      case (r, c) =>
//        val (i, rn) = r.nextInt
//
//        Option(((i, rn), (rn, c - 1)))
//    }.unzip
//
//    (l, r_out.last)

    LazyList
      .unfold(rng)(_.nextInt.pure[Option].map(t => (t, t._2)))
      .take(count)
      .toList
      .unzip
      .map(_.last)

//    if (count == 0) (List.empty[Int], rng)
//    else {
////      val (tail, rt) = ints(count - 1)(rng)
////      val (head, rh) = (rt).nextInt
////
////      (head :: tail, rh)
//
//      println("hello")
//
//      val (head, rh) = rng.nextInt
//      val (tail, rt) = ints(count - 1)(rh)
//
//      (head :: tail, rt)
//    }
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
