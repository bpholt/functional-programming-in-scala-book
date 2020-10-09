package fpinscala.testing

import Prop._
import fpinscala.state.RNG.{map, nonNegativeLessThan}
import fpinscala.state._

trait Prop { self =>
  def check(): Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(p: Prop): Prop = new Prop {
    override def check(): Either[(FailedCase, SuccessCount), SuccessCount] =
      self.check().flatMap { selfSuccess =>
        p.check() match {
          case Right(sc) => Right(selfSuccess + sc)
          case Left((fc, sc)) => Left((fc, selfSuccess + sc))
        }
      }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](a: Gen[A])
               (f: A => Boolean): Prop = ???
}

case class Gen[A](sample: State[RNG, A]) {
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
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(map(nonNegativeLessThan(stopExclusive - start))(_ + start))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen {
    map(nonNegativeLessThan(2))(_ == 0)
  }

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
}

object PropApp extends App {
//  val p1: Prop = () => Right(2)
//  val p2: Prop = () => Left(("oops2", 3))
//
//  println((p1 && p2).check())

  println(Gen.listOfN(10, Gen.boolean).sample.run(SimpleRNG(5L)))

  println(Gen.boolean.sample.run(SimpleRNG(5L)))
  println(Gen.boolean.sample.run(SimpleRNG(126074519596L)))
  println(Gen.boolean.sample.run(SimpleRNG(54580536946886L)))
  println(Gen.boolean.sample.run(SimpleRNG(48179997485145L)))
  println(Gen.boolean.sample.run(SimpleRNG(128185544502587L)))
  println(Gen.boolean.sample.run(SimpleRNG(50918106956842L)))
  println(Gen.boolean.sample.run(SimpleRNG(93306604150977L)))
  println(Gen.boolean.sample.run(SimpleRNG(11020690987064L)))
  println(Gen.boolean.sample.run(SimpleRNG(54766004951253L)))
  println(Gen.boolean.sample.run(SimpleRNG(120186769387708L)))

  println(Gen.boolean.listOfN(Gen.unit(10)).sample.run(SimpleRNG(5L)))

}
