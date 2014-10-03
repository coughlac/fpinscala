package gettingstarted

import fpinscala.gettingstarted.PolymorphicFunctions.isSorted

class IsSortedTest extends org.specs2.mutable.Specification{
  "A SortedArray" should {
    "be fully sorted" in {
      isSorted(Array(2, 3, 6, 7, 8, 9), (a: Int, b: Int) => a > b) must beFalse
      isSorted(Array(9, 8, 2, 6, 3, 7), (a: Int, b: Int) => a > b) must beFalse
      isSorted(Array(2, 9, 6, 3, 7, 8), (a: Int, b: Int) => a > b) must beFalse
      isSorted(Array(9, 8, 7, 6, 3, 2), (a: Int, b: Int) => a > b) must beTrue

      isSorted(Array(2, 3, 6, 7, 8, 9), (a: Int, b: Int) => a < b) must beTrue
      isSorted(Array(9, 8, 2, 6, 3, 7), (a: Int, b: Int) => a < b) must beFalse
      isSorted(Array(2, 9, 6, 3, 7, 8), (a: Int, b: Int) => a < b) must beFalse
      isSorted(Array(9, 8, 7, 6, 3, 2), (a: Int, b: Int) => a < b) must beFalse

      isSorted(Array('a', 'b', 'd', 'e', 'h', 'z'), (a: Char, b: Char) => a > b) must beFalse
      isSorted(Array('z', 'a', 'b', 'd', 'e', 'h'), (a: Char, b: Char) => a > b) must beFalse
      isSorted(Array('a', 'd','b', 'e', 'h'), (a: Char, b: Char) => a > b) must beFalse
      isSorted(Array('z', 'x','w', 'e', 'c', 'a'), (a: Char, b: Char) => a > b) must beTrue
    }
  }
}
