package fpinscala.errorhandling

import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case s: Some[A] => Some(f(s.get))
    case _ => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case s: Some[B] => s.get
    case _ => default
  }
  //the answer impl where they do not use a case statement looks rubbish
  def filter(f: A => Boolean): Option[A] = this match {
    case s: Some[A] if f(s.get) => s
    case _ => None
  }
//-------------------------------------------------------
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(avg => mean(xs.map(x => math.pow(x - avg, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aVal <- a
      bVal <- b
    } yield f(aVal, bVal)
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldLeft[Option[List[A]]](Some(Nil))((acc: Option[List[A]], elem: Option[A]) => map2(elem, acc)(_ :: _)).map(_.reverse)

  def seq2[A,B](zero: B)(a: List[Option[A]])(f: (A, B) => B): Option[B] = a match {
    case Nil => Some(zero)
    case head :: rest => map2(head, seq2(zero)(rest)(f))(f)
  }
  //map over a list using a function that might fail,
  //returning None if applying it to any element of the list returns None
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    //sequence(a.map(f))
    case Nil => Some(Nil)
    case head :: tail => map2(f(head), traverse(tail)(f))(_ :: _)
  }

  def Try[A](a: => A): Option[A] = {
    try Some(a) catch {
      case e: Exception => None
    }
  }
}