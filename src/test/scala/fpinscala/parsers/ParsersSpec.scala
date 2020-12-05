package fpinscala.parsers

import cats._
import cats.syntax.all._
import eu.timepit.refined.scalacheck.all._
import eu.timepit.refined.types.all.NonEmptyString
import fpinscala.parsers.ParsersSpec.{CharactersParsed, Parser}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

object ParsersSpec {
  type CharactersParsed = Int
  type Parser[+A] = String => Either[String, (CharactersParsed, A)]
}

class ParsersSpec
  extends AnyFlatSpec
    with ScalaCheckPropertyChecks
    with Configuration
    with Matchers
    with Parsers[String, Parser] {

  override implicit def string(s: String): String => Either[String, (CharactersParsed, String)] =
    input =>
      if(input.startsWith(s)) Right((s.length, s))
      else Left(s"Expected «$s» but got «$input»")

  override def run[A](p: Parser[A])(input: String): Either[String, A] =
    p(input).map(_._2)

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = input =>
    s1(input).orElse(s2(input))

  override def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] = input =>
    for {
      (ia, a) <- pa(input)
      (ib, b) <- pb(input.substring(ia))
    } yield (ia + ib, (a, b))

  override def map[A, B](a: Parser[A])(f: A => B): Parser[B] = input =>
    a(input).map(_.map(f))

  override def slice[A](p: Parser[A]): Parser[String] = input =>
    p(input).map {
      case (c, _) => (c, input.substring(0, c))
    }

  behavior of "Parser[Char]"

  it should "parse a character" in {
    forAll { c: Char =>
      run(char(c))(c.toString) should be(Right(c))
    }
  }

  behavior of "Parser[String]"

  it should "parse a String" in {
    forAll { s: String =>
      run(string(s))(s) should be(Right(s))
    }
  }

  behavior of "or"

  it should "combine two string parsers" in {
    forAll { (s1: String, s2: String) =>
      whenever(s1.nonEmpty) {
        run(s1 or s2)(s1) should be(Right(s1))
        run(s1 or s2)(s2) should be(Right(s2))
      }
    }
  }

  implicit def arbRepeated[A: Arbitrary : Monoid]: Arbitrary[(Int, A)] = Arbitrary {
    for {
      i <- Gen.chooseNum(2, 10)
      a <- arbitrary[A]
    } yield (i, a combineN i)
  }

  behavior of "map"

  it should "obey the identity law" in {
    forAll { s: String =>
      run(string(s).map(identity))(s) should be(run(string(s))(s))
    }
  }

  behavior of "succeed"

  it should "always succeed" in {
    forAll { (i: Int, input: String) =>
      run(succeed(i))(input) should be(Right(i))
    }
  }

  behavior of "slice"

  it should "do the book's example" in {
    run(slice((char('a') or char('b')).many))("aaba") should be(Right("aaba"))
  }

  behavior of "listOfN"

  it should "recognize the examples from the book" in {
    run(listOfN(3, "ab" | "cad"))("ababcad").map(_.mkString) should be(Right("ababcad"))
    run(listOfN(3, "ab" | "cad"))("cadabab").map(_.mkString) should be(Right("cadabab"))
    run(listOfN(3, "ab" | "cad"))("ababab").map(_.mkString) should be(Right("ababab"))
  }

  it should "recognize repeated strings" in { // TODO combine this with or
    forAll(arbRepeated[String].arbitrary) { case (count: Int, s: String) =>
      whenever(s.nonEmpty && count >= 0) {
        run(listOfN(count, s))(s combineN count) == Right(s combineN count)
      }
    }
  }

  behavior of "product"

  it should "combine the two things" in {
    forAll(Gen.chooseNum(0, 1000)) { (i: Int) =>
      val str = "a" combineN i
      run(string(str) ** countAs)(str) should be(Right((str, 0)))
    }
  }

  behavior of "many"

  it should "recognize 1 to 10 things" in {
    forAll(Gen.chooseNum(1, 10)) { count =>
      forAll { s: NonEmptyString =>
        val repeatedS = s.value combineN count
        run(many(repeatedS))(repeatedS) should be(Right(List(repeatedS)))
      }
    }
  }
}
