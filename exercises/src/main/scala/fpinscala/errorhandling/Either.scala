package fpinscala.errorhandling


import scala.{Option ⇒ _, Either ⇒ _, Left ⇒ _, Right ⇒ _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  def map[B](f: A ⇒ B): Either[E, B] = this match {
    case Right(a) ⇒ new Right(f(a))
    case l: Left[E] => l
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = map(f) match {
    case Right(s) ⇒ s
    case Left(e) ⇒ new Left(e)
  }

  def orElse[EE >: E, B >: A](b: ⇒ Either[EE, B]): Either[EE, B] = this match {
    case r: Right[A] ⇒ r
    case l: Left[E] ⇒ b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) ⇒ C): Either[EE, C] = for {
    aVal ← this
    bVal ← b
  } yield f(aVal, bVal)
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def Try[A](a: () => A): Either[Exception, A] = try Right(a())
  catch { case e: Exception => Left(e) }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def traverse[E, A, B](as: List[A])(f: A ⇒ Either[E, B]): Either[E, List[B]] = {
    as.foldRight[Either[E, List[B]]](Right(Nil))((a, b) ⇒ f(a).map2(b)(_ :: _))
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(elem ⇒ elem)
}