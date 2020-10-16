package fpinscala.testing

case class SGen[A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] = SGen { i =>
    val ga: Gen[A] = forSize(i)

    ga.map(f)
  }
/*
    SGen(i => forSize(i).map(f))
*/

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
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(Gen.listOfN(_, g))
/*
    SGen(i => g.listOfN(Gen.unit(i)))
*/
}
