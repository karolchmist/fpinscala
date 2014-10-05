package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(vl, vr) => maximum(vl) max maximum(vr)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(t1, t2) => 1 + (depth(t1) max depth(t2))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(t1, t2) => Branch(map(t1)(f), map(t2)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B,B)=>B): B = t match {
    case Leaf(v) => f(v)
    case Branch(t1,t2) => g(fold(t1)(f)(g),fold(t2)(f)(g))
  }

}
