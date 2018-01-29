package com.planetholt.fpinscala.laziness

import Stream._

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption_a: Option[A] = this match {
    case Empty ⇒ None
    case Cons(h, _) ⇒ Some(h())
  }

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) ⇒ Option(a))

  def toList: List[A] = this match {
    case Empty ⇒ List.empty
    case Cons(h, t) ⇒ h() +: t().toList
  }

  def take_a(n: Int): Stream[A] = if (n < 1) Stream.empty else this match {
    case Empty ⇒ Empty
    case Cons(h, t) ⇒ cons(h(), t().take(n - 1))
  }

  def take(n: Int): Stream[A] = unfold(n → this) {
    case (i, Cons(h, t)) if i > 0 ⇒ Option((h(), (i - 1) → t()))
    case _ ⇒ None
  }

  def foldRight[B](z: ⇒ B)(f: (A, ⇒ B) ⇒ B): B = this match {
    case Cons(h, t) ⇒ f(h(), t().foldRight(z)(f))
    case _ ⇒ z
  }

  def map_a[B](f: A ⇒ B): Stream[B] = foldRight(empty[B])((a, bs) ⇒ cons(f(a), bs))

  def map[B](f: A ⇒ B): Stream[B] = unfold(this) {
    case Cons(h, t) ⇒ Option((f(h()), t()))
    case Empty ⇒ None
  }

  def filter(f: A ⇒ Boolean): Stream[A] = foldRight(empty[A])((h, t) ⇒ if (f(h)) cons(h, t) else t)

  def append[B>:A](s: ⇒ Stream[B]): Stream[B] = foldRight(s)((h, t) ⇒ cons(h, t))

  def flatMap[B](f: A ⇒ Stream[B]): Stream[B] = foldRight(empty[B])((h, t) ⇒ f(h).append(t))

  def find(p: A ⇒ Boolean): Option[A] = filter(p).headOption

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 ⇒ t().drop(n - 1)
    case Cons(_, _) if n <= 0 ⇒ this
    case _ ⇒ Empty
  }

  def takeWhile(p: A ⇒ Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) ⇒ Option(h(), t())
    case _ ⇒ None
  }

  def takeWhile_b(p: A ⇒ Boolean): Stream[A] = foldRight(Stream.empty[A]) { (a, b) ⇒
    if (p(a)) cons(a, b)
    else empty
  }

  def takeWhile_a(p: A ⇒ Boolean): Stream[A] = this match {
    case Empty ⇒ Empty
    case Cons(h, t) if p(h()) ⇒ cons(h(), t().takeWhile_a(p))
    case Cons(h, _) if !p(h()) ⇒ Empty
  }

  final def exists(p: A ⇒ Boolean): Boolean = foldRight(false)((a, b) ⇒ p(a) || b)

  def forAll(p: A ⇒ Boolean): Boolean = foldRight(true)((a, b) ⇒ p(a) && b)

  def zipWith[B, C](bs: Stream[B])(f: (A, B) ⇒ C): Stream[C] = unfold(this → bs) {
    case (Cons(h1, t1), Cons(h2, t2)) ⇒ Option(f(h1(), h2()) → (t1() → t2()))
    case _ ⇒ None
  }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this → bs) {
    case (Cons(h1, t1), Cons(h2, t2)) ⇒ Option(Option(h1()) → Option(h2()) → (t1() → t2()))
    case (Empty, Cons(h2, t2)) ⇒ Option((None, Option(h2())) → (empty[A] → t2()))
    case (Cons(h1, t1), Empty) ⇒ Option((Option(h1()), None) → (t1() → empty[B]))
    case _ ⇒ None
  }

  def startsWith[B >: A](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h, h2) => h == h2
    }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty ⇒ None
    case s ⇒ Option(s → (s drop 1))
  } append Stream(empty)

  def hasSubsequence[B >: A](s: Stream[B]): Boolean = tails exists(_ startsWith s)

  def scanRight[B](z0: ⇒ B)(f: (A, ⇒ B) ⇒ B): Stream[B] =
    foldRight(z0 → Stream(z0)) { (a, b) ⇒
      b match {
        case (z, s) ⇒
          val intermediate = f(a, z)
          intermediate → cons(intermediate, s)
      }
    }._2

  override def toString: String = s"Stream(${this.toList.mkString(", ")})"
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () ⇒ A, t: () ⇒ Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: ⇒ A, tl: ⇒ Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() ⇒ head, () ⇒ tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones_a: Stream[Int] = cons(1, ones_a)

  val ones: Stream[Int] = unfold(1)(_ ⇒ Option((1, 1)))

  def constant_a[A](a: A): Stream[A] = cons(a, constant_a(a))

  def constant[A](a: A): Stream[A] = unfold(a)(i ⇒ Option((i, i)))

  def from_a(n: Int): Stream[Int] = cons(n, from_a(n + 1))

  def from(n: Int): Stream[Int] = unfold(n)(i ⇒ Option((i, i + 1)))

  val fibs_a: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a+b))
    go(0, 1)
  }

  val fibs: Stream[Int] = unfold(0 → 1) {
    case (a, b) ⇒ Option(a → (b, a + b))
  }

  def unfold[A, S](z: S)(f: S ⇒ Option[(A, S)]): Stream[A] = f(z).fold(empty[A]) {
    case (a, s) ⇒ cons(a, unfold(s)(f))
  }

  def unfold_a[A, S](z: S)(f: S ⇒ Option[(A, S)]): Stream[A] = f(z) match {
    case None ⇒ Stream.empty[A]
    case Some((a, s)) ⇒ cons(a, unfold(s)(f))
  }
}
