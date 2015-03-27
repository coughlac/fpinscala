package gettingstarted

import org.specs2.mutable.Specification
import fpinscala.errorhandling.{Option, Some, None}

class OptionTest extends Specification {

  "Option getOrElse" should {
    "return the value if the option has some value or the default value if the option has none " in {
      val someValue: Option[Int] = Some(5)
      someValue.getOrElse("has no number") must beEqualTo(5)

      val noneValue: Option[Int] = None
      noneValue.getOrElse("has no number") must beEqualTo("has no number")
    }
  }
  "Option orElse" should {
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
}
