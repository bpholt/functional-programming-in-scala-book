package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](list: List[A]): List[A] = list match {
    case Cons(_, tail) => tail
    case Nil => Nil
  }

  def setHead[A](list: List[A], head: A): List[A] = list match {
    case Cons(_, tail) => Cons(head, tail)
    case Nil => Cons(head, Nil)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, tail) if n == 1 => tail
    case Cons(_, tail) if n > 1 => drop(tail, n - 1)
    case Nil => Nil
  }

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) | Nil => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

}
