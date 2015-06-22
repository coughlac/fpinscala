package state

import fpinscala.state.RNG

class StateTest extends org.specs2.mutable.Specification {

  "nonNegativeNumber - generate a random number between 0 and Int.maxValue inclusive, using RNG.nextInt" should {
    "return a positive int" in {
      val (value, _) = RNG.nonNegativeInt(stubRng(-42))
      value should beEqualTo(42)
    }
    "return a positive int for Int.maxValue" in {
      val (value, _) = RNG.nonNegativeInt(stubRng(Int.MaxValue))
      value should beEqualTo(Int.MaxValue)
    }
    "return a positive int for 0" in {
      val (value, _) = RNG.nonNegativeInt(stubRng(0))
      value should beEqualTo(0)
    }
    "return Int.maxValue for Int.minValue" in {
      val (value, _) = RNG.nonNegativeInt(stubRng(Int.MinValue))
      value should beEqualTo(Int.MaxValue)
    }
  }

  "double" should {
    "return 0 " in {
      val (value, _) = RNG.double(stubRng(Int.MinValue))
      value should beEqualTo(0D)
    }

    "return 0 instead of 1" in {
      val (value, _) = RNG.double(stubRng(Int.MaxValue))
      value should beEqualTo(0D)
    }

    "return numbers between 0 and 1" in {
      val (value, _) = RNG.double(stubRng(Int.MaxValue / 2))
      value should beEqualTo(0.5D)
    }
  }

  "intDouble - generate a tuple with a random Int and a random Double" should {
    "should always return a positive int" in {
      val ((i, d), _) = RNG.intDouble(stubRng(-42))
      i should beEqualTo(-42)
      d should beEqualTo(0.01)
    }
    "should always return a positive int for Int.maxValue" in {
      val ((i, d), _) = RNG.intDouble(stubRng(Int.MaxValue))
      i should beEqualTo(Int.MaxValue)
      d should beEqualTo(0.0D)
    }
    "should always return a positive int for 0" in {
      val ((i, d), _) = RNG.intDouble(stubRng(0))
      i should beEqualTo(0)
      d should beEqualTo(0.0D)
    }
    "should always return Int.maxValue for Int.minValue" in {
      val ((i, d), _) = RNG.intDouble(stubRng(Int.MinValue))
      i should beEqualTo(Int.MinValue)
      d should beEqualTo(0.0D)
    }
  }

  "ints" should {
    "generate a list of random ints" in {
      val fixture = stubRng(Int.MinValue)
      val (listOfRandomInts, _) = RNG.ints(5)(fixture)

      listOfRandomInts should beEqualTo( Int.MinValue :: Int.MinValue :: Int.MinValue :: Int.MinValue :: Int.MinValue :: Nil)
      fixture.getCallCount() should beEqualTo(5)
    }
  }

  "doubleAlt" should {
    "return 0 " in {
      val (value, _) = RNG.doubleAlt(stubRng(Int.MinValue))
      value should beEqualTo(0D)
    }

    "return 0 instead of 1" in {
      val (value, _) = RNG.doubleAlt(stubRng(Int.MaxValue))
      value should beEqualTo(0D)
    }

    "return numbers between 0 and 1" in {
      val (value, _) = RNG.doubleAlt(stubRng(Int.MaxValue / 2))
      value should beEqualTo(0.5D)
    }
  }

  private def stubRng(value: Int) = {
    var callCount: Int = 0
    new RNG() {
      override def nextInt: (Int, RNG) = {
        callCount+=1
        (value, this)
      }

      def getCallCount(): Int = callCount
    }
  }
}