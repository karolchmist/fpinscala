package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil => throw new IllegalStateException("Can't tail on Nil")
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case Nil => throw new IllegalStateException("Can't setHead on Nil")
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => throw new IllegalStateException
      case Cons(_, t) => drop(t, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    case Nil => Nil
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, append(t, l2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def foldLeft[A, B](l: List[A], acc: B)(f: (A, B) => B): B = l match {
    case Nil => acc
    case Cons(h, t) => foldLeft(t, f(h, acc))(f)
  }

  def sum2(l: List[Int]): Int = foldRight(l, 0)(_ + _)

  def product2(l: List[Int]): Int = foldRight(l, 1)((a, b) => a * b)

  def length2[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def sumLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productLeft(l: List[Int]): Int = foldLeft(l, 1)((a, b) => a * b)

  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((_, acc) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((a: A, acc: List[A]) => Cons(a, acc))

  def append2[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)((a: A, b: List[A]) => Cons(a, b))

  def concat[A](ll: List[List[A]]): List[A] =
    foldRight(ll, Nil: List[A])(append2)

  def incr[A](l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((a, acc) => Cons(a + 1, acc))

  def toStr[A](l: List[Int]): List[String] =
    foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((a, acc) => Cons(f(a), acc))

  def filter[A](l: List[A])(p: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, t) => if (p(h)) Cons(h, t) else t)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil: List[B])((a: A, b: List[B]) => append(f(a), b))

  def flatMap2[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterUsingflatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l) { a: A => if (f(a)) Cons(a, Nil) else Nil}

  def sumLists(l1:List[Int], l2:List[Int]) : List[Int] = (l1,l2) match {
    case (_,Nil) => Nil
    case (Nil,_) => Nil
    case (Cons(h1,t1),Cons(h2,t2)) => Cons(h1+h2,sumLists(t1,t2))
  }

  def zipWith[A,B,C](l1:List[A],l2:List[B])(f: (A,B) => C):List[C] = (l1,l2) match {
    case (_,Nil) => Nil
    case (Nil,_) => Nil
    case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2),zipWith(t1,t2)(f))
  }

}