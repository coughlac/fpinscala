package state

import fpinscala.state.RNG

class StateTest extends org.specs2.mutable.Specification {

  "nonNegativeNumber - generate a random number between 0 and Int.maxValue inclusive, using RNG.nextInt" should {
    "should always return a positive int" in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (-42, this)
      }
      val (value, _) = RNG.nonNegativeInt(testRNG)
      value should beEqualTo(42)
    }
    "should always return a positive int for Int.maxValue" in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (Int.MaxValue, this)
      }
      val (value, _) = RNG.nonNegativeInt(testRNG)
      value should beEqualTo(Int.MaxValue)
    }
    "should always return a positive int for 0" in {
      val testRNG: RNG = new RNG() {
        override def nextInt: (Int, RNG) = (0, this)
      }
      val (value, _) = RNG.nonNegativeInt(testRNG)
      value should beEqualTo(0)
    }
    "should always return Int.maxValue for Int.minValue" in {
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
}
