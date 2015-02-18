package gettingstarted

import fpinscala.datastructures._

class ListTest extends org.specs2.mutable.Specification {

  "Tail of empty list should return an empty list" should {
    "be correct" in {
      List.tail(Nil) must beEqualTo(Nil)
    }
  }

  "Tail of list should remove head of list" should {
    "be correct" in {
      List.tail(Cons(1, Cons(2, Nil))) must beEqualTo(Cons(2, Nil))
    }
  }

  "setHead of an empty list should update head of list with specified value" should {
    "be correct" in {
      List.setHead(Nil, 2) must beEqualTo(Cons(2, Nil))
    }
  }

  "setHead of list should update head of list with specified value" should {
    "be correct" in {
      List.setHead(Cons(1, Cons(2, Nil)), 3) must beEqualTo(Cons(3, Cons(2, Nil)))
    }
  }
}
