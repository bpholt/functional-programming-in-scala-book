package com.planetholt.fpinscala.errorhandling

sealed trait Option[+A] {
  def map[B](f: A ⇒ B): Option[B]
  def getOrElse[B >: A](default: ⇒ B): B

  def flatMap[B](f: A ⇒ Option[B]): Option[B] = map(f) getOrElse None
  def orElse[B >: A](obj: ⇒ Option[B]): Option[B] = this map(Some(_)) getOrElse obj
  def filter(f: A ⇒ Boolean): Option[A] = flatMap (b ⇒ if (f(b)) this else None)
}

object Option {
  def apply[A](a: A): Option[A] = if (null == a) None else Some(a)

  def lift[A, B](f: A ⇒ B): Option[A] ⇒ Option[B] = _ map f

  def map2[A, B, C](maybeA: Option[A], maybeB: Option[B])(f: (A, B) ⇒ C): Option[C] = for {
    a ← maybeA
    b ← maybeB
  } yield f(a, b)

  def traverse[A, B](a: List[A])(f: A ⇒ Option[B]): Option[List[B]] =
    a.foldRight(Option(List.empty[B]))((a, acc) ⇒ map2(f(a), acc)(_ +: _))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)

  def Try[A](a: ⇒ A): Option[A] = try Some(a) catch {
    case _: Exception ⇒ None
  }
}

case class Some[+A](get: A) extends Option[A] {
  override def map[B](f: A ⇒ B): Option[B] = Some(f(get))

  override def getOrElse[B >: A](default: ⇒ B): B = get
}
case object None extends Option[Nothing] {
  override def map[B](f: Nothing ⇒ B): Option[B] = None

  override def getOrElse[B >: Nothing](default: ⇒ B): B = default
}

object Variance {
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m ⇒ mean(xs.map(x ⇒ math.pow(x - m, 2))))
}
