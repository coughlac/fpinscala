package state

import fpinscala.state.RNG
import fpinscala.state.RNG.Rand

class StateTest extends org.specs2.mutable.Specification {

  "nonNegativeNumber - generate a random number between 0 and Int.maxValue inclusive, using RNG.nextInt" should {
    "return a positive int" in {
      val (value, _) = RNG.nonNegativeInt(SpyRng(-42))
      value should beEqualTo(42)
    }
    "return a positive int for Int.maxValue" in {
      val (value, _) = RNG.nonNegativeInt(SpyRng(Int.MaxValue))
      value should beEqualTo(Int.MaxValue)
    }
    "return a positive int for 0" in {
      val (value, _) = RNG.nonNegativeInt(SpyRng(0))
      value should beEqualTo(0)
    }
    "return Int.maxValue for Int.minValue" in {
      val (value, _) = RNG.nonNegativeInt(SpyRng(Int.MinValue))
      value should beEqualTo(Int.MaxValue)
    }
  }

  "double" should {
    "return 0 " in {
      val (value, _) = RNG.double(SpyRng(Int.MinValue))
      value should beEqualTo(0D)
    }

    "return 0 instead of 1" in {
      val (value, _) = RNG.double(SpyRng(Int.MaxValue))
      value should beEqualTo(0D)
    }

    "return numbers between 0 and 1" in {
      val (value, _) = RNG.double(SpyRng(Int.MaxValue / 2))
      value should beEqualTo(0.5D)
    }
  }

  "intDouble - generate a tuple with a random Int and a random Double" should {
    "should always return a positive int" in {
      val ((i, d), _) = RNG.intDouble(SpyRng(-42))
      i should beEqualTo(-42)
      d should beEqualTo(0.01)
    }
    "should always return a positive int for Int.maxValue" in {
      val ((i, d), _) = RNG.intDouble(SpyRng(Int.MaxValue))
      i should beEqualTo(Int.MaxValue)
      d should beEqualTo(0.0D)
    }
    "should always return a positive int for 0" in {
      val ((i, d), _) = RNG.intDouble(SpyRng(0))
      i should beEqualTo(0)
      d should beEqualTo(0.0D)
    }
    "should always return Int.maxValue for Int.minValue" in {
      val ((i, d), _) = RNG.intDouble(SpyRng(Int.MinValue))
      i should beEqualTo(Int.MinValue)
      d should beEqualTo(0.0D)
    }
  }

  "ints" should {
    "generate a list of random ints" in {
      val fixture = SpyRng(Int.MinValue)
      val (listOfRandomInts, _) = RNG.ints(5)(fixture)

      listOfRandomInts should beEqualTo( Int.MinValue :: Int.MinValue :: Int.MinValue :: Int.MinValue :: Int.MinValue :: Nil)
      fixture.getCallCount should beEqualTo(5)
    }
  }

  "intsSeq" should {
    "generate a list of random ints" in {
      val fixture = SpyRng(Int.MinValue)
      val a: Rand[List[Int]] = RNG.intsSeq(5)
      val listOfRandomInts = a(fixture)._1
      listOfRandomInts should beEqualTo( Int.MinValue :: Int.MinValue :: Int.MinValue :: Int.MinValue :: Int.MinValue :: Nil)
      fixture.getCallCount should beEqualTo(5)
    }
  }

  "doubleAlt" should {
    "return 0 " in {
      val (value, _) = RNG.doubleAlt(SpyRng(Int.MinValue))
      value should beEqualTo(0D)
    }

    "return 0 instead of 1" in {
      val (value, _) = RNG.doubleAlt(SpyRng(Int.MaxValue))
      value should beEqualTo(0D)
    }

    "return numbers between 0 and 1" in {
      val (value, _) = RNG.doubleAlt(SpyRng(Int.MaxValue / 2))
      value should beEqualTo(0.5D)
    }
  }

  "map2" should {
    "return a tuple with f applied to value a from ra and b from rb as the value and the resultant RNG " in {
      val ra: (RNG) => (Int, RNG) = aRand => aRand.nextInt
      val rb: (RNG) => (Int, RNG) = bRand => bRand.nextInt
      val a: Rand[Int] = RNG.map2( ra , rb)((a, b) => a + b)
      a(SpyRng(20))._1 should beEqualTo(40)
    }
  }

  case class SpyRng(value: Int) extends RNG {
    private var callCount: Int = 0

    override def nextInt: (Int, RNG) = {
      callCount += 1
      (value, this)
    }

    def getCallCount: Int = callCount
  }
}