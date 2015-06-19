package state

import fpinscala.state.RNG

class StateTest extends org.specs2.mutable.Specification {

  "nonNegativeNumber - generate a random number between 0 and Int.maxValue inclusive, using RNG.nextInt" should {
    "should return a positive int" in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (-42, this)
      }
      val (value, _) = RNG.nonNegativeInt(testRNG)
      value should beEqualTo(42)
    }
    "should return a positive int for Int.maxValue" in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (Int.MaxValue, this)
      }
      val (value, _) = RNG.nonNegativeInt(testRNG)
      value should beEqualTo(Int.MaxValue)
    }
    "should return a positive int for 0" in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (0, this)
      }
      val (value, _) = RNG.nonNegativeInt(testRNG)
      value should beEqualTo(0)
    }
    "should return Int.maxValue for Int.minValue" in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (Int.MinValue, this)
      }
      val (value, _) = RNG.nonNegativeInt(testRNG)
      value should beEqualTo(Int.MaxValue)
    }
  }

  "double" should {
    "return 0 " in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (Int.MinValue, this)
      }
      val (value, _) = RNG.double(testRNG)
      value should beEqualTo(0D)
    }

    "return 0 instead of 1" in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (Int.MaxValue, this)
      }
      val (value, _) = RNG.double(testRNG)
      value should beEqualTo(0D)
    }

    "return numbers between 0 and 1" in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (Int.MaxValue / 2, this)
      }
      val (value, _) = RNG.double(testRNG)
      value should beEqualTo(0.5D)
    }
  }

  "intDouble - generate a tuple with a random Int and a random Double" should {
    "should always return a positive int" in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (-42, this)
      }
      val ((i, d), _) = RNG.intDouble(testRNG)
      i should beEqualTo(-42)
      d should beEqualTo(0.01)
    }
    "should always return a positive int for Int.maxValue" in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (Int.MaxValue, this)
      }
      val ((i, d), _) = RNG.intDouble(testRNG)
      i should beEqualTo(Int.MaxValue)
      d should beEqualTo(0.0D)
    }
    "should always return a positive int for 0" in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (0, this)
      }
      val ((i, d), _) = RNG.intDouble(testRNG)
      i should beEqualTo(0)
      d should beEqualTo(0.0D)
    }
    "should always return Int.maxValue for Int.minValue" in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (Int.MinValue, this)
      }
      val ((i, d), _) = RNG.intDouble(testRNG)
      i should beEqualTo(Int.MinValue)
      d should beEqualTo(0.0D)
    }
  }

  "ints" should {
    "generate a list of random ints" in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (Int.MinValue, this)
      }
      val (listOfRandomInts, _) = RNG.ints(5)(testRNG)
      listOfRandomInts should beEqualTo( Int.MinValue :: Int.MinValue :: Int.MinValue :: Int.MinValue :: Int.MinValue :: Nil)
    }
  }

  "doubleAlt" should {
    "return 0 " in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (Int.MinValue, this)
      }
      val (value, _) = RNG.doubleAlt(testRNG)
      value should beEqualTo(0D)
    }

    "return 0 instead of 1" in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (Int.MaxValue, this)
      }
      val (value, _) = RNG.doubleAlt(testRNG)
      value should beEqualTo(0D)
    }

    "return numbers between 0 and 1" in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (Int.MaxValue / 2, this)
      }
      val (value, _) = RNG.doubleAlt(testRNG)
      value should beEqualTo(0.5D)
    }
  }
}