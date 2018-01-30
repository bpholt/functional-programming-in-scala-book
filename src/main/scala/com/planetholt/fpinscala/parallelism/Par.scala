package com.planetholt.fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Future, TimeUnit}

import scala.language.higherKinds

object Par {
  type Par[A] = ExecutorService ⇒ Future[A]

  def unit[A](a: A): Par[A] = _ ⇒ UnitFuture(a)

  def lazyUnit[A](a: ⇒ A): Par[A] = fork(unit(a))

  implicit class ParOps[A](a: Par[A]) {
    def map2[B, C](b: Par[B])(f: (A, B) ⇒ C): Par[C] = es ⇒ {
      val af = a(es)
      val bf = b(es)

      UnitFuture(f(af.get, bf.get)) // ignores timeouts
    }

    def map[B](f: A ⇒ B): Par[B] = map2(unit())((a, _) ⇒ f(a))

    def run(ec: ExecutorService): A =
      a(ec).get()
  }

  def parMap[A, B](ps: List[A])(f: A ⇒ B): Par[List[B]] = fork {
    sequence(ps.map(asyncF(f)))
  }

  def parFilter[A](ps: List[A])(f: A ⇒ Boolean): Par[List[A]] =
    sequence(ps.map(asyncF(a ⇒ if (f(a)) List(a) else List()))).map(_.flatten)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(List.empty[A]))((p, l) ⇒ p.map2(l)(_ +: _))

  def fork[A](a: ⇒ Par[A]): Par[A] = es ⇒ es.submit(() => a(es).get)

  def asyncF[A, B](f: A ⇒ B): A ⇒ Par[B] = a ⇒ lazyUnit(f(a))

  def reduce[A](as: IndexedSeq[A])(f: (A, A) ⇒ A): Par[Option[A]] =
    if (as.size <= 1)
      unit(as.headOption)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      val reduceL = reduce(l)(f)
      val reduceR = reduce(r)(f)

      fork(reduceL).map2(fork(reduceR))((a, b) ⇒ for {
        aa ← a
        bb ← b
      } yield f(aa, bb))
    }

  def max(is: IndexedSeq[Int]): Par[Option[Int]] = reduce(is)(_ max _)

  def wordCount(paragraphs: List[String]): Par[Int] = ???

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) ⇒ D): Par[D] =
    a.map2(b)((a, b) ⇒ (c: C) ⇒ f(a, b, c)).map2(c)(_ (_))

  def delay[A](fa: ⇒ Par[A]): Par[A] = fa

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def isCancelled: Boolean = false

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get
}

object Examples {

  import Par._

  //noinspection SimplifiableFoldOrReduce
  def sum(ints: Seq[Int]): Int = ints.foldLeft(0)((a, b) ⇒ a + b)

  def sum_a(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption.getOrElse(0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum_a(l) + sum_a(r)
    }

  //  def sum_b(ints: IndexedSeq[Int]): Int =
  //    if (ints.size <= 1)
  //      ints.headOption.getOrElse(0)
  //    else {
  //      val (l, r) = ints.splitAt(ints.length / 2)
  //      val sumL: Par[Int] = Par.unit(sum_b(l))
  //      val sumR: Par[Int] = Par.unit(sum_b(r))
  //
  //      Par.get(sumL) + Par.get(sumR)
  //    }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = unit(sum(l))
      val sumR: Par[Int] = unit(sum(r))

      fork(sumL).map2(fork(sumR))(_ + _)
    }

}
