package com.planetholt.fpinscala.errorhandling

sealed trait Either[+E, +A] {
  def map[B](f: A ⇒ B): Either[E, B] = this match {
    case Left(ex) ⇒ Left(ex)
    case Right(a) ⇒ Right(f(a))
  }

  def flatMap[EE >: E, B](f: A ⇒ Either[EE, B]): Either[EE, B] = this match {
    case Left(ex) ⇒ Left(ex)
    case Right(a) ⇒ f(a)
  }

  def orElse[EE >: E, B >: A](b: ⇒ Either[EE, B]): Either[EE, B] = this match {
    case Left(_) ⇒ b
    case Right(_) ⇒ this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) ⇒ C): Either[EE, C] = for {
    aa ← this
    bb ← b
  } yield f(aa, bb)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: ⇒ A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception ⇒ Left(e)
    }

  def right[E, A](a: A): Either[E, A] = Right(a)

  def traverse[E, A, B](as: List[A])(f: A ⇒ Either[E, B]): Either[E, List[B]] =
    as.foldRight(right[E, List[B]](List.empty[B]))((a, acc) ⇒ f(a).map2(acc)(_ +: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)
}
