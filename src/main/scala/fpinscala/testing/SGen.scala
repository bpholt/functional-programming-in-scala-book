package fpinscala.testing

import fpinscala.parallelism.Par._
import fpinscala.state.{RNG, State}

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(i: Int): Gen[A] = forSize(i)

  def map[B](f: A => B): SGen[B] = SGen { i =>
    val ga: Gen[A] = forSize(i)

    ga.map(f)
  }

  def flatMap[B](f: A => SGen[B]): SGen[B] =
// book answer key version
//  {
//    val g2: Int => Gen[B] = n => {
//      this.forSize(n) flatMap { f(_).forSize(n) }
//    }
//
//    SGen(g2)
//  }

    SGen { i =>
      val g: A => Gen[B] =
        (a: A) => f(a).forSize(i)

      this.forSize(i).flatMap(g)
    }

  /*
      SGen { i =>
        forSize(i).flatMap(f(_).forSize(i))
      }
  */
  def map2[B, C](g: SGen[B])
                (f: (A, B) => C): SGen[C] =
    for {
      a <- this
      b <- g
    } yield f(a, b)

  def **[B](g: SGen[B]): SGen[(A,B)] =
    (this map2 g)((_,_))
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(Gen.listOfN(_, g))

  def listOf1[A](g: Gen[A]) = nonEmptyList(g)
  def nonEmptyList[A](g: Gen[A]): SGen[List[A]] =
//    SGen(n => Gen.listOfN(n max 1, g))
    SGen { n =>
      for {
        a <- g
        as <- Gen.listOfN(n, g)
      } yield a :: as
    }

  val parInt: SGen[Par[Int]] = SGen {
    case 0 => Gen.choose(0, 20).map(i => unit(i))
    case i: Int =>
      val value: Gen[Par[Int]] = parInt.forSize(i - 1)

//      val x: State[RNG, Par[Int]] =
//        for {
//          r <- State.get
//          v <- value.sample
//        } yield {
//          println(r)
//          v
//        }

      /*Gen(x)*/
      value.map(fork(_))
  }
}
