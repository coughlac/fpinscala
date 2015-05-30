package gettingstarted

import fpinscala.laziness.{Cons, Empty}

class LazyTest extends org.specs2.mutable.Specification {
  "toList" should {
    "return Nil if stream is empty" in {
      Empty.toList should beEqualTo(Nil)
    }
    "return the elements of the stream as a list if not empty" in {
      Cons(() ⇒ "A",  () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty))).toList should beEqualTo("A" :: "B" :: "C" :: Nil)
    }
  }
}
