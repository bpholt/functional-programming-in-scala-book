package fpinscala.testing

import fpinscala.state._
import fpinscala.testing.Prop._

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (n, rng) =>
    run(n, rng) match {
      case Passed => p.run(n, rng)
      case x@Falsified(_, _) => x
    }
  }
  def ||(p: Prop): Prop = Prop { (n, rng) =>
    run(n, rng) match {
      case Passed => Passed
      case Falsified(_, _) => p.run(n, rng)
    }
  }
/*
    Prop { (n, rng) =>
      run(n, rng) match {
        case Passed => Passed
        case _ => p.run(n, rng)
      }
    }
*/
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  def forAll[A](as: Gen[A])
               (f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(as)(rng)
      .zip(LazyList.from(0))
      .take(n)
      .map {
        case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(buildMsg(a, e), i)
          }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])
                     (rng: RNG): LazyList[A] =
    LazyList.unfold(rng) { rng =>
      Some(g.sample.run(rng))
    }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

sealed trait Result extends Product with Serializable {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  def isFalsified = true
}


object PropApp extends App {
//  val p1: Prop = () => Right(2)
//  val p2: Prop = () => Left(("oops2", 3))
//
//  println((p1 && p2).check())

//  println(Gen.listOfN(10, Gen.boolean).sample.run(SimpleRNG(5L)))

//  println(Gen.boolean.sample.run(SimpleRNG(5L)))
//  println(Gen.boolean.sample.run(SimpleRNG(126074519596L)))
//  println(Gen.boolean.sample.run(SimpleRNG(54580536946886L)))
  println(Gen.boolean.sample.run(SimpleRNG(48179997485145L)))
  println(Gen.boolean.sample.run(SimpleRNG(128185544502587L)))
//  println(Gen.boolean.sample.run(SimpleRNG(50918106956842L)))
//  println(Gen.boolean.sample.run(SimpleRNG(93306604150977L)))
//  println(Gen.boolean.sample.run(SimpleRNG(11020690987064L)))
//  println(Gen.boolean.sample.run(SimpleRNG(54766004951253L)))
//  println(Gen.boolean.sample.run(SimpleRNG(120186769387708L)))

//  println(Gen.boolean.listOfN(Gen.unit(10)).sample.run(SimpleRNG(5L)))

}
