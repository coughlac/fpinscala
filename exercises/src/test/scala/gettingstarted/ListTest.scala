package gettingstarted

import fpinscala.datastructures._

class ListTest extends org.specs2.mutable.Specification {

  "Tail of empty list" should {
    "return an empty list" in {
      List.tail(Nil) must beEqualTo(Nil)
    }
  }

  "Tail of list" should {
    "remove head of list" in {
      List.tail(Cons(1, Cons(2, Nil))) must beEqualTo(Cons(2, Nil))
    }
  }

  "setHead of an empty list" should {
    "update head of list with specified value" in {
      List.setHead(Nil, 2) must beEqualTo(Cons(2, Nil))
    }
  }

  "setHead of list" should {
    "update head of list with specified value" in {
      List.setHead(Cons(1, Cons(2, Nil)), 3) must beEqualTo(Cons(3, Cons(2, Nil)))
    }
  }

  "drop of an empty list" should {
    "not change the original list" in {
      List.drop(Nil, 3) must beEqualTo(Nil)
    }
  }

  "drop 0 elements" should {
    "not change the original list" in {
      List.drop(Cons(1, Cons(2, Nil)), 0) must beEqualTo(Cons(1, Cons(2, Nil)))
    }
  }

  "drop n elements of list" should {
    "take n elements from the head of the list" in {
      List.drop(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), 3) must beEqualTo(Cons(4, Nil))
    }
  }

  "dropWhile of an empty list" should {
    "not change the original list" in {
      List.dropWhile(Nil, (x: Int) => x < 5) must beEqualTo(Nil)
    }
  }

  "dropWhile" should {
    "not change the original list if the predicate is false for all elements" in {
      List.dropWhile(Cons(1, Cons(2, Nil)), (x: Int) => x > 3) must beEqualTo(Cons(1, Cons(2, Nil)))
    }
  }

  "dropWhile" should {
    "empty a list if the predicate is true for all of its elements" in {
      List.dropWhile(Cons(1, Cons(2, Nil)), (x: Int) => x < 3) must beEqualTo(Nil)
    }
  }

  "dropWhile" should {
    "take elements from the head of the list until the predicate is false for one of them or the list is empty" in {
      List.dropWhile(Cons(1, Cons(2, Cons(3, Cons(4, Cons(2, Nil))))), (x: Int) => x < 3) must beEqualTo(Cons(3, Cons(4, Cons(2, Nil))))
    }
  }

  "dropWhile of an empty list" should {
    "not change the original list" in {
      List.dropWhile(Nil, (x: Int) => x < 5) must beEqualTo(Nil)
    }
  }

  "init" should {
    "not change an empty list" in {
      List.init(Nil) must beEqualTo(Nil)
    }
  }

  "init" should {
    "remove last element from list" in {
      List.init(Cons(1, Cons(2, Nil))) must beEqualTo(Cons(1, Nil))
      List.init(Cons(1, Cons(2, Cons(3, Cons(4, Cons(2, Nil)))))) must beEqualTo(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
    }
  }
}