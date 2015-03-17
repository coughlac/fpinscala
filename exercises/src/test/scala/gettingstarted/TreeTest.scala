package gettingstarted

import fpinscala.datastructures.{TreeHelper, Branch, Leaf}

class TreeTest extends org.specs2.mutable.Specification{
  "size" should {
    "return the number of nodes (leaves and branches) in the tree" in {
      TreeHelper.size( Leaf("a"), 0) must beEqualTo(1)
      TreeHelper.size( Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d"))), 0) must beEqualTo(7)
      TreeHelper.size( Branch(Branch(Leaf("a"), Leaf("b")), Leaf("d")), 0) must beEqualTo(5)
      TreeHelper.size( Branch(Branch(Leaf("a"), Leaf("b")), Leaf("d")), 0) must beEqualTo(5)
    }
  }
}
