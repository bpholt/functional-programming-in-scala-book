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
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
//    rng => {
//      val (i, r) = nonNegativeInt(rng)
//      (i / (Int.MaxValue.toDouble + 1), r)
//    }
/*
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
*/

  def intDouble: Rand[(Int, Double)] =
//    rng => {
//      val (i, r) = rng.nextInt
//      val (d, r2) = double(r)
//
//      ((i, d), r2)
//    }
    both(int, double)

  def doubleInt: Rand[(Double, Int)] =
//    rng => {
//      val (d, r) = double(rng)
//      val (i, r2) = r.nextInt
//
//      ((d, i), r2)
//    }
  both(double, int)

  def double3: Rand[(Double, Double, Double)] = rng => {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

/*
  def ints(count: Int): Rand[List[Int]] = {
    import cats.implicits._

    LazyList
      .unfold(_)(_.nextInt.pure[Option].map(t => (t, t._2)))
      .take(count)
      .toList
      .unzip
      .map(_.last)
  }
*/
  def ints(count: Int): Rand[List[Int]] =
//  rng => {
//    val (li, lr) = LazyList
//      .unfold(rng)(r => Option(r.nextInt).map(t => (t, t._2)))
//      .take(count)
//      .toList
//      .unzip
//
//    (li, lr.last)
//  }
  sequence(List.fill(count)(int))
/*
    sequence(List.fill(count)(int))
*/

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
//    rng => (a, rng)
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
/*
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)

      (f(a, b), r2)
    }
*/

  def sequence[A](fs: LazyList[Rand[A]]): Rand[LazyList[A]] =
    fs.foldRight(unit(LazyList.empty[A]))(map2(_, _)(_ #:: _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
//   def foldRight[B](z: B)(op: (A, B) => B): B

//    fs.foldRight[Rand[List[A]]](unit(List.empty[A])) { case (ra: Rand[A], ras: Rand[List[A]]) =>
//      map2(ra, ras)(_ :: _)
//    }
    fs.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))
    // def foldLeft[B](z: B)(op: (B, A) => B): B
//    fs.foldLeft[Rand[List[A]]](unit(List.empty[A])) { case (ras: Rand[List[A]], ra: Rand[A]) =>
////      map2(ra, ras)(_ :: _)
//      map2(ras, ra)((as, a) => a :: as)
//    }

//    fs match {
//      case Nil => // unit(List.empty[A])
//         rng => (List.empty[A], rng)
//
// foldRight:
//      case (h: Rand[A]) :: (t: List[Rand[A]]) =>
//        rng => {
//          val (a: A, r1) = h(rng)
//          val (as: List[A], r2) = sequence(t)(r1)
//
//          (a :: as, r2)
//        }
// foldLeft:
//      case (h: Rand[A]) :: (t: List[Rand[A]]) =>
//        rng => {
//          val (as: List[A], r1) = sequence(t)(rng)
//          val (a: A, r2) = h(r1)
//
//          (a :: as, r2)
//        }
//    }
  }
/*
    fs.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))
*/
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
