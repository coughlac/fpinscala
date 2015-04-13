package gettingstarted

import org.specs2.mutable.Specification
import fpinscala.errorhandling.{Option, Some, None}

class OptionTest extends Specification {

  "getOrElse" should {
    "return the value if the option has some value or the default value if the option has none " in {
      Some(5).getOrElse("has no number") must beEqualTo(5)

      None.getOrElse("has no number") must beEqualTo("has no number")
    }
  }
  "orElse" should {
    "return the first Option if its defined otherwise it returns the second Option" in {
      val secondWithSome: Some[Int] = Some(15)
      val secondWithNone: Option[Int] = None

      val firstWithSome: Option[Int] = Some(5)
      firstWithSome.orElse(secondWithSome) must beEqualTo(firstWithSome)
      firstWithSome.orElse(secondWithNone) must beEqualTo(firstWithSome)

      val firstWithNone: Option[Int] = None
      firstWithNone.orElse(secondWithSome) must beEqualTo(secondWithSome)
      firstWithNone.orElse(secondWithNone) must beEqualTo(secondWithNone)
    }
  }

  "filter" should {
    "return the Option with Some if the predicate is true for the Option's value else None " in {
      val some: Option[Int] = Some(5)
      val none: Option[Int] = None
      some.filter(x => x > 0) must beEqualTo(some)
      some.filter(x => x > 100) must beEqualTo(None)
      none.filter(x => x > 0) must beEqualTo(None)
      none.filter(x => x > 10) must beEqualTo(None)
    }
  }

  "map" should {
    "return the Option with f applied to Some if there is one else None " in {
      val x: Int = 5
      val some: Option[Int] = Some(x)
      val none: Option[Int] = None
      val msg = "string value is: "
      val f: (Int) => String = x => msg + x

      some.map(f) must beEqualTo(Some(msg + x))
      none.map(f) must beEqualTo(None)
    }
  }

  "flatMap" should {
    "return the Option with f applied to Some if there is one else None " in {
      val x: Int = 5
      val some: Option[Int] = Some(x)
      val none: Option[Int] = None
      val msg = "string value is: "
      val f: (Int) => Option[String] = x => Some(msg + x)

      some.flatMap(f) must beEqualTo(Some(msg + x))
      none.flatMap(f) must beEqualTo(None)
    }
  }

  "variance" should {
    "return the mean of (x-m)^2 where x is each element in xs and m is the mean of each element in xs" in {
      val input: Seq[Double] = Seq(1.0, 2.0, 3.0, 4.0, 5.0)

      Option.variance(input) must beEqualTo(Some(2.0))
    }
  }

  "map2" should {
    "return a new Option[C] of the function f applied to Option[A] and Option[B]." in {
      val optionA: Option[Double] = Some(2.0)
      val optionB: Option[Char] = Some('A')
      val f: (Double, Char) => String = (a: Double, b: Char) => s"A: $a and B: $b"

      Option.map2(optionA, None)(f) must beEqualTo(None)
      Option.map2(None, optionB)(f) must beEqualTo(None)
      Option.map2(optionA, optionB)(f) must beEqualTo(Some("A: 2.0 and B: A"))
    }
  }

  "seq2" should {
    "return a new Option[List[A]] from a List[Option[A]] which is None if any of the list of option." in {
      val optionA: List[Option[Int]] = List(Some(1), Some(2))
      val optionB: List[Option[Int]] = List(Some(1), None, Some(2))

      Option.seq2(0)(optionA)(_ + _) must beEqualTo(Some(3))
      Option.seq2(0)(optionB)(_ + _) must beEqualTo(None)
    }
  }

  "sequence" should {
    "return a new Option[List[A]] from a List[Option[A]] which is None if any of the list of option." in {
      val optionA: List[Option[Int]] = List(Some(1), Some(2))
      val optionB: List[Option[Int]] = List(Some(1), None, Some(2))

      Option.sequence(optionA) must beEqualTo(Some(List(1,2)))
      Option.sequence(optionB) must beEqualTo(None)
    }
  }

  "traverse" should {
    "return a new Option[List[B]] from a List[A] which is None if " +
      "any of the list elements is None after applying f." in {
      val list: List[String] = List("1", "2")
      val badList: List[String] = List("z", "2")
      val emptyList: List[String] = List()

      Option.traverse[String, Int](list)(x => Option.Try(x.toInt)) must beEqualTo(Some(List(1, 2)))
      Option.traverse[String, Int](badList)(x => Option.Try(x.toInt)) must beEqualTo(None)
      Option.traverse[String, Int](emptyList)(x => Option.Try(x.toInt)) must beEqualTo(Some(emptyList))
    }
  }
}
