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

case class Gen[A](sample: State[RNG, A])

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
}
