package gettingstarted

import fpinscala.datastructures.{TreeHelper, Branch, Leaf}

class TreeTest extends org.specs2.mutable.Specification{
  "size" should {
    "return the number of nodes (leaves and branches) in the tree" in {
      TreeHelper.size( Leaf("a")) must beEqualTo(1)
      TreeHelper.size( Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) must beEqualTo(7)
      TreeHelper.size( Branch(Branch(Leaf("a"), Leaf("b")), Leaf("d"))) must beEqualTo(5)
      TreeHelper.size( Branch(Branch(Leaf("a"), Leaf("b")), Leaf("d"))) must beEqualTo(5)
    }
  }

  "maximum" should {
    "return the maximum element of nodes (leaves and branches) in the tree of ints" in {
      TreeHelper.maximum( Leaf(2)) must beEqualTo(2)
      TreeHelper.maximum( Branch(Branch(Leaf(30), Leaf(50)), Branch(Leaf(70), Leaf(90)))) must beEqualTo(90)
      TreeHelper.maximum( Branch(Branch(Leaf(4), Leaf(9)), Leaf(3))) must beEqualTo(9)
      TreeHelper.maximum( Branch(Branch(Leaf(4), Leaf(5)), Leaf(1))) must beEqualTo(5)
    }
  }

  "map" should {
    "return the maximum element of nodes (leaves and branches) in the tree of ints" in {
      TreeHelper.map( Leaf(2))(i => i + 1) must beEqualTo(Leaf(3))
      TreeHelper.map( Branch(Branch(Leaf(30), Leaf(50)), Branch(Leaf(70), Leaf(90))))(i => i + 1) must beEqualTo(Branch(Branch(Leaf(31), Leaf(51)), Branch(Leaf(71), Leaf(91))))
    }
  }
}
