package gettingstarted

import fpinscala.errorhandling.{Either, Right, Left}
import org.specs2.mutable.Specification

class EitherTest extends Specification {

  "orElse" should {
    "return the first Either if its defined otherwise it returns the second Either" in {

      val x: Int = 5
      val success: Either[String, Int] = Right(x)
      val error: Either[String, Int] = Left("error occurred")
      val msg = "string value is: "
      val f: Either[String, String] = Right(msg + x)
      success.orElse(f) must beEqualTo(success)
      error.orElse(f) must beEqualTo(f)
    }
  }

//  "filter" should {
//    "return the Option with Some if the predicate is true for the Option's value else None " in {
//      val some: Option[Int] = Some(5)
//      val none: Option[Int] = None
//      some.filter(x => x > 0) must beEqualTo(some)
//      some.filter(x => x > 100) must beEqualTo(None)
//      none.filter(x => x > 0) must beEqualTo(None)
//      none.filter(x => x > 10) must beEqualTo(None)
//    }
//  }

  "map" should {
    "return the Either with f applied to the Right if there is one else Left " in {
      val x: Int = 5
      val success: Either[String, Int] = Right(x)
      val error: Either[String, Int] = Left("error occurred")
      val msg = "string value is: "
      val f: (Int) => String = x => msg + x
      success.map(f) must beEqualTo(Right(msg + x))
      error.map(f) must beEqualTo(error)
    }
  }

  "flatMap" should {
    "return the Option with f applied to Some if there is one else None " in {
      val x: Int = 5
      val success: Either[String, Int] = Right(x)
      val error: Either[String, Int] = Left("error occurred")
      val msg = "string value is: "
      val f: (Int) => Either[String, String] = x => Right(msg + x)

      success.flatMap(f) must beEqualTo(Right(msg + x))
      error.flatMap(f) must beEqualTo(error)
    }
  }

//  "variance" should {
//    "return the mean of (x-m)^2 where x is each element in xs and m is the mean of each element in xs" in {
//      val input: Seq[Double] = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
//
//      Option.variance(input) must beEqualTo(Some(2.0))
//    }
//  }
//
  "map2" should {
    "return a new Either[C] of the function f applied to Either[A] and Either[B]." in {
      val eitherA: Either[String, Double] = Right(2.0)
      val eitherB: Either[String, Char] = Right('A')
      val f: (Double, Char) => String = (a: Double, b: Char) => s"A: $a and B: $b"

      val errorEither = Left("error occurred.")
      eitherA.map2(errorEither)(f) must beEqualTo(errorEither)
      errorEither.map2(eitherB)(f) must beEqualTo(errorEither)
      eitherA.map2(eitherB)(f) must beEqualTo(Right("A: 2.0 and B: A"))
    }
  }
//
//  "seq2" should {
//    "return a new Option[List[A]] from a List[Option[A]] which is None if any of the list of option." in {
//      val optionA: List[Option[Int]] = List(Some(1), Some(2))
//      val optionB: List[Option[Int]] = List(Some(1), None, Some(2))
//
//      Option.seq2(0)(optionA)(_ + _) must beEqualTo(Some(3))
//      Option.seq2(0)(optionB)(_ + _) must beEqualTo(None)
//    }
//  }
//
  "sequence" should {
    "return a new Option[List[A]] from a List[Option[A]] which is None if any of the list of option." in {
      val eitherA: List[Either[String, Int]] = List(Right(1), Right(2))
      val error: Left[String] = Left("Error occurred.")
      val eitherB: List[Either[String, Int]] = List(Right(1), error, Right(2))

      Either.sequence(eitherA) must beEqualTo(Right(List(1,2)))
      Either.sequence(eitherB) must beEqualTo(error)
    }
  }
//
//  "traverse" should {
//    "return a new Option[List[B]] from a List[A] which is None if " +
//      "any of the list elements is None after applying f." in {
//      val list: List[String] = List("1", "2")
//      val badList: List[String] = List("z", "2")
//      val emptyList: List[String] = List()
//
//      Option.traverse[String, Int](list)(x => Option.Try(x.toInt)) must beEqualTo(Some(List(1, 2)))
//      Option.traverse[String, Int](badList)(x => Option.Try(x.toInt)) must beEqualTo(None)
//      Option.traverse[String, Int](emptyList)(x => Option.Try(x.toInt)) must beEqualTo(Some(emptyList))
//    }
//  }
}
