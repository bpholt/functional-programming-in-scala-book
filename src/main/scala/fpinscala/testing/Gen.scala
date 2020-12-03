package fpinscala.testing

import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import fpinscala.state.RNG._
import fpinscala.state._

//case class Gen[+A](sample: State[RNG,A]) {

case class Gen[A](sample: State[RNG, A]) {
  def unsized: SGen[A] =
    SGen(_ => this)
/*
    SGen(_ => this)
*/

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen {
    //    for {
    //      a <- sample
    //      b <- f(a).sample
    //    } yield b

    sample.flatMap { a =>
      f(a).sample
    }
  }
  /*
      Gen {
        sample.flatMap(f(_).sample)
      }
  */

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap { i =>
      Gen(State.sequence(List.fill(i)(sample)))
    }
  //    size.flatMap { i =>
  //      val value: Gen[List[A]] = Gen.listOfN(i, this)
  //      value
  //    }
  /*
      size.flatMap(Gen.listOfN(_, this))
  */
  /*
      size.flatMap { n =>
        Gen {
          State.sequence(List.fill(n)(sample))
        }
      }
  */

  def map2[B, C](g: Gen[B])
                (f: (A, B) => C): Gen[C] =
    for {
      a <- this
      b <- g
    } yield f(a, b)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g) ((_, _))
}

object Gen {
//  def choose(start: Int, stopExclusive: Int): Gen[Int] =
//    Gen(map(nonNegativeLessThan(stopExclusive - start))(_ + start))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(RNG.nonNegativeInt.map(n => (start.toLong + n.toLong % (stopExclusive.toLong - start.toLong)).toInt))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen {
    map(nonNegativeLessThan(2))(_ == 0)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(unit(n)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen {
    State.sequence(List.fill(n)(g.sample))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)
  //    boolean.ifM(g1, g2)
  //    boolean.flatMap {
  //      case true => g1
  //      case false => g2
  //    }

  def tuple[A, B](ga: Gen[A], gb: Gen[B]): Gen[(A, B)] =
    for {
      a <- ga
      b <- gb
    } yield (a, b)
  //    ga.flatMap(a => gb.flatMap(b => unit((a, b))))

  val double: Gen[Double] = Gen(RNG.double)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    double.flatMap { d =>
      val total = g1._2 + g2._2
      val g1Weight = g1._2 / total
      //      val g2Weight = g2._2 / total

      if (g1Weight < d) g1._1 else g2._1
    }

  val S: Gen[java.util.concurrent.ExecutorService] =
    weighted(
      choose(1, 4).map(java.util.concurrent.Executors.newFixedThreadPool) -> .75,
      unit(java.util.concurrent.Executors.newCachedThreadPool) -> .25
    )

  val int = Gen.choose(Int.MinValue, Int.MaxValue)

  /* A `Gen[Par[Int]]` generated from a list summation that spawns a new parallel
 * computation for each element of the input list summed to produce the final
 * result. This is not the most compelling example, but it provides at least some
 * variation in structure to use for testing.
 *
 * Note that this has to be a `lazy val` because of the way Scala initializes objects.
 * It depends on the `Prop` companion object being created, which references `pint2`.
 */
  lazy val pint2: Gen[Par[Int]] =
    choose(-100, 100)
      .listOfN(choose(0, 20))
      .map { l =>
        l.foldLeft(Par.unit(0)) { (p, i) =>
          Par.fork {
            Par.map2(p, Par.unit(i))(_ + _)
          }
        }
      }

}
