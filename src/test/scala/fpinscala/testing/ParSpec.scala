package fpinscala.testing

import fpinscala.parallelism.Par
import fpinscala.parallelism.Par._
import fpinscala.testing.Gen._
import fpinscala.testing.Prop._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParSpec extends AnyFlatSpec with Matchers {

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  private def forAllPar[A](ga: Gen[A])
                  (f: A => Par[Boolean]): Prop =
    forAll(S ** ga) {
      case s ** a =>
        Par.run(s)(f(a))
    }

  private def forAllPar[A](ga: SGen[A])
                  (f: A => Par[Boolean]): Prop =
    forAll(SGen(_ => S) ** ga) {
      case s ** a =>
        Par.run(s)(f(a))
    }

  private def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

//  "the example test" should "check that Par.map works" in {
//    val p2 = checkPar { equal (
//      Par.map(Par.unit(1))(_ + 1),
//      Par.unit(2) )
//    }
//
//    Prop.runForScalaTest(p2) should be(Passed)
//  }

  "Par" should "be the same whether forked or not" in  {
    val p3 = forAllPar(SGen.parInt) { pi =>
      equal(pi, fork(pi))
    }

    Prop.runForScalaTest(p3) should be(Passed)
  }

  "Par" should "be the same whether forked or not using the book's generator" in  {
    val p4 = forAllPar(Gen.pint2) { pi =>
      equal(pi, fork(pi))
    }

    Prop.runForScalaTest(p4) should be(Passed)
  }

  "takeWhile" should "obey the takeWhile law" in {
//    type A = Int
//    def s: List[A] = ???
//    def f: A => Boolean = ???
//
//    s.takeWhile(f).forall(f) should be (true)
//
//    s.takeWhile(f).length <= s.length
//
////    s.takeWhile(f).length + s.dropWhile(f).length == s.length
//    s.takeWhile(f) ++ s.dropWhile(f) == s
//
//    s.dropWhile(f).exists(a => !f(a)) || s.dropWhile(f).isEmpty
//
//    s.dropWhile(f).headOption.exists(f) should be(false)

    val isEven = (i: Int) => i % 2 == 0
    val takeWhileProp =
      Prop.forAll(Gen.listOf(int))(ns => ns.takeWhile(isEven).forall(isEven))

    Prop.runForScalaTest(takeWhileProp) should be(Passed)

  }
// Gen[String => Int]

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
    g.map(i => (_ => i))
}
