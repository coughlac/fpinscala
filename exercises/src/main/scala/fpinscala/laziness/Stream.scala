package fpinscala.laziness

import fpinscala.laziness.Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n != 0 ⇒ Cons(h, () ⇒ t().take(n - 1))
    case _ ⇒ Empty
  }

  def takeAlt(n: Int): Stream[A] = {
    Stream.unfold(this) {
      case Cons(h, t) if n != 0 ⇒ Some(h(), t().takeAlt(n - 1))
      case _ ⇒ None
    }
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty ⇒ Empty
    case Cons(h, t) if n == 0 ⇒ this
    case Cons(h, t) ⇒ t().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((h, acc) => if (p(h)) cons(h, acc) else empty)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((h, acc) => p(h) && acc)

  //  Optionally selects the first element.
  //
  //    Note: might return different results for different runs, unless the underlying collection type is ordered.
  //  returns
  //  the first element of this traversable collection if it is nonempty, None if it is empty.
  def headOption: Option[A] = foldRight(None: Option[A])((h, acc) ⇒ Some(h))

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, acc) => cons[B](f(h), acc))

  def mapAlt[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(head, tail) ⇒ Some(f(head()), tail())
      case _ ⇒ None
    }
  }

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, acc) => (h, acc) match {
    case (head, s) if p(head) => cons(head, s)
    case _ => acc
  })

  def append[B >: A](a2: Stream[B]): Stream[B] = foldRight(a2)((h, acc) => cons(h, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, acc) => f(h).append(acc))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, constant(1))
  val onesAlt: Stream[Int] = Stream.unfold(1) { _ ⇒ Some(1, 1) }

  def constant[B](b: B): Stream[B] = cons(b, constant(b))

  def constantAlt[B](b: B): Stream[B] = Stream.unfold(b) { case x ⇒ Some(x, x) }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fromAlt(n: Int): Stream[Int] = Stream.unfold(n) { case x ⇒ Some(x, x + 1) }

  def fibs(n: Int): Stream[Int] = {
    def generateNextFibonacciNumber(prev: Int, current: Int): Stream[Int] = {
      val next = prev + current
      cons(prev, generateNextFibonacciNumber(current, next))
    }
    generateNextFibonacciNumber(n, n + 1)
  }

  def fibsAlt(n: Int): Stream[Int] = Stream.unfold((n, n + 1)) {
    case (prev, current) ⇒ Some((prev, (current, prev + current)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def loop(result: Option[(A, S)]): Stream[A] = result match {
      case None ⇒ empty
      case Some((a, s)) ⇒ cons(a, loop(f(s)))
    }
    loop(f(z))
  }
}