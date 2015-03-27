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
}
