package fpinscala.parsers

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)]
  def map[A, B](a: Parser[A])(f: A => B): Parser[B]
  def slice[A](p: Parser[A]): Parser[String]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    map2(p, listOfN(n - 1, p))(_ :: _) or succeed(List.empty[A])

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List.empty[A])

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
    (pa ** pb).map(f.tupled)

  val countAs: Parser[Int] = char('a').many.slice.map(_.length)

  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def **[B](pb: => Parser[B]): Parser[(A, B)] = self.product(p, pb)
    def product[B](pb: => Parser[B]): Parser[(A, B)] = self.product(p, pb)
  }

}
