package com.planetholt.fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldOps {
  implicit class TreeFoldOps[A](val tree: Tree[A]) extends AnyVal {
    def fold[B](f: A ⇒ B)(g: (B, B) => B): B = tree match {
      case Leaf(a) ⇒ f(a)
      case Branch(l, r) ⇒ g(l.fold(f)(g), r.fold(f)(g))
    }

    def size: Int = tree.fold(_ ⇒ 1)(_ + _ + 1)

    def depth: Int = tree.fold(_ ⇒ 0)(_ max _ + 1)

    def map[B](f: A ⇒ B): Tree[B] = tree.fold[Tree[B]](a ⇒ Leaf(f(a)))(Branch(_, _))
  }

  implicit class IntTreeOps(val tree: Tree[Int]) extends AnyVal {
    def maximum: Int = tree.fold(identity)(_ max _)
  }

}

object FoldlessTreeOps {

  implicit class TreeOps[A](val tree: Tree[A]) extends AnyVal {

    // exercise 3.25
    def size: Int = tree match {
      case Leaf(_) ⇒ 1
      case Branch(l, r) ⇒ l.size + r.size + 1
    }

    // exercise 3.27
    def depth: Int = tree match {
      case Leaf(_) ⇒ 0
      case Branch(l, r) ⇒ (l.depth max r.depth) + 1
    }

    // exercise 3.28
    def map[B](f: A ⇒ B): Tree[B] = tree match {
      case Leaf(a) ⇒ Leaf(f(a))
      case Branch(l, r) ⇒ Branch(l.map(f), r.map(f))
    }
  }

  implicit class IntTreeOps(val tree: Tree[Int]) extends AnyVal {

    // exercise 3.26
    def maximum: Int = tree match {
      case Leaf(i) ⇒ i
      case Branch(l, r) ⇒ l.maximum max r.maximum
    }

  }

}
