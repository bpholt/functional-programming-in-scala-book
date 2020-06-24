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
  def nonNegativeInt(rng: RNG): (Int, RNG) = ???
  def double(rng: RNG): (Double, RNG) = ???
  def intDouble(rng: RNG): ((Int, Double), RNG) = ???
  def doubleInt(rng: RNG): ((Double, Int), RNG) = ???
  def double3(rng: RNG): ((Double, Double, Double), RNG) = ???
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
