package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = foldLeft(ints, 0)((acc, elem) => acc + elem)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)((acc, elem) => elem * acc)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(i, Cons(2, Cons(4, _))) => i
    case Nil => 42
    case Cons(i, Cons(j, Cons(3, Cons(4, _)))) => i + j
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((e, acc) => Cons(e, acc))

  //foldLeft(reverse(a1), a2)((acc, e)=> Cons(e, acc))


  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = drop(l, 1)

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case _ => Cons(h, tail(l))
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(h, t) if n > 0 => drop(t, n - 1)
    case _ => l
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {
    val list: List[A] = l match {
      case Nil => l
      case _ => reverseWithFilter(l, Nil, (x: List[A]) => x != Nil)
    }
    reverseWithFilter(list, Nil, (x: List[A]) => true)
  }

  @tailrec
  def reverseWithFilter[A](l: List[A], copy: List[A], f: List[A] => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(t) =>
        reverseWithFilter(t, Cons(h, copy), f)
      case _ => copy
    }
  }

  def length[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1) //foldRight(l, 0)((_, acc) => 1 + acc)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(l), z)((h, z) => f(z, h))

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((z, h) => f(h, z))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight2(l, Nil: List[B])((elem, ls) => Cons(f(elem), ls))

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((t, h) => Cons(h, t))

  def flatMap2[A](l: List[List[A]]): List[A] = foldRight2(l, Nil: List[A])(append)

  def incrementBy1(l: List[Int]): List[Int] = foldRight2(l, Nil: List[Int])((elem, ls) => Cons(elem + 1, ls))

  def doubleToString(list: List[Double]): List[String] = foldRight2(list, Nil: List[String])((elem, ls) => Cons(elem.toString, ls))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight2(as, Nil: List[A])((a: A, ls: List[A]) => if (f(a)) Cons(a, ls) else ls)

  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((a: A) => if (f(a)) Cons(a, Nil) else Nil: List[A])
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    flatMap2(map(l)(f))
  }

  def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = (xs, ys) match {
    case (Nil, Nil) => Nil
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  @tailrec
  def isMatch[A](partialSup: List[A], supRemainder: List[A], sub: List[A]): Boolean = (partialSup, supRemainder, sub) match {
    case (_, Nil, _) => false
    case (h, _, s) if reverse(h) == s => true
    case (_, Cons(h2, t2), subSeq) => isMatch(Cons(h2, partialSup), t2, subSeq)
  }

  @tailrec
  def isMatch2[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Cons(h1, _), Cons(h2, _)) if h1 != h2 => false
    case (Cons(_, Nil), Cons(_, _)) => false
    case (Cons(_, _), Cons(_, Nil)) => true
    case (Cons(_, t1), Cons(_, t2)) => isMatch2(t1, t2)
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Nil, Nil) => false
    case (Nil, _) => false
    case (_, Nil) => false
    case (Cons(h, t), _) if isMatch2(sup, sub) => true
    //case (Cons(h, t), _) if isMatch(Cons(h, Nil), t, sub) => true
    case (Cons(h, t), _) => hasSubsequence(t, sub)
  }
}
