package fpinscala.testing

import Prop._
import fpinscala.state.RNG.{map, nonNegativeLessThan}
import fpinscala.state._

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (n, rng) =>
//    this.run(n, rng) match {
//      case Passed => p.run(n, rng.nextInt._2)
//      case f => f
//    }


    val s1 = State[RNG, Result] { r =>
      val (_, nextRng) = r.nextInt

      (this.run(n, r), nextRng)
    }
    val s2 = State[RNG, Result] { r =>
      val (_, nextRng) = r.nextInt

      (p.run(n, r), nextRng)
    }

    val (rs: List[Result], _) = State.sequence(List(s1, s2)).run(rng)

//    val r1: Result = this.run(n, rng)
//    val r2: Result = p.run(n, rng)

    val r1 = rs.head
    val r2 = rs(1)

    (r1, r2) match {
      case (Passed, Passed) => Passed
      case (Passed, Falsified(f, s)) => Falsified(f, s + n)
      case (Falsified(f, s), Passed) => Falsified(f, s + n)
      case (Falsified(f1, s1), Falsified(f2, s2)) =>
        Falsified(f1 + "\n" + f2, s1 + s2)
    }
  }
  def ||(p: Prop): Prop = ???
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
