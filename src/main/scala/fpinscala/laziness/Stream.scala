package fpinscala.laziness

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => List.empty[A]
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  @tailrec final def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case Cons(_, _) if n <= 0 => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = ???

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = ???

  def map[B](f: A => B): Stream[B] = ???
  def filter(pred: A => Boolean): Stream[A] = ???
  def append[AA >: A](as: Stream[AA]): Stream[AA] = ???
  def flatMap[B](f: A => Stream[B]): Stream[B] = ???
}
case object Empty extends Stream[Nothing]
final case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def onesPlus: Stream[Int] = Cons(() => 0, () => ones)
  def ones: Stream[Int] = Cons(() => 1, () => ones)

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
