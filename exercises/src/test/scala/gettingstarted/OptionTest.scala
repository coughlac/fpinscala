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
}
