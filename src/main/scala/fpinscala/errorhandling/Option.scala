package fpinscala.errorhandling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(get) => get
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)
  def filter(f: A => Boolean): Option[A] = map(f).flatMap(b => if(b) this else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def map2[A,B,C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- oa
      b <- ob
    } yield f(a, b)

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(List.empty[A]) : Option[List[A]]) {
      case (Some(list), Some(a)) => Some(a :+ list)
      case _ => None
    }

}

object Variance {
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] =
    for {
      m: Double <- mean(xs)
      v <- mean(xs.map(x => math.pow(x - m, 2)))
    } yield v
//    mean(xs)
//      .flatMap(m =>
//        mean(xs.map(x => math.pow(x - m, 2)))
//          .map(v => v)
//      )
}
