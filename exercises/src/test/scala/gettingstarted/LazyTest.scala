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

  "take" should {
    "not change the original stream if it is empty" in {
      Empty.take(3) must beEqualTo(Empty)
    }
  }

  "take of 0 elements" should {
    "return an Empty Stream" in {
      val actualStream = Cons(() ⇒ "A",  () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))
      actualStream.take(0) must beEqualTo(Empty)
    }
  }

  "take n elements of stream" should {
    "take n elements from the head of the stream" in {
      val actualStream = Cons(() ⇒ "A",  () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))
      val expectedStream = Cons(() ⇒ "A",  () ⇒ Cons(() ⇒ "B", ()⇒ Empty))
      actualStream.take(2).toList must beEqualTo(expectedStream.toList)
    }
  }

  "take more elements of stream than it has" should {
    "return the original stream" in {
      val actualStream = Cons(() ⇒ "A",  () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))
      val expectedStream = actualStream
      actualStream.take(4).toList must beEqualTo(expectedStream.toList)
    }
  }

  "drop of an empty stream" should {
    "not change the original stream" in {
      Empty.drop(3) must beEqualTo(Empty)
    }
  }

  "drop 0 elements" should {
    "not change the original stream" in {
      val actualStream = Cons(() ⇒ "A",  () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))
      actualStream.drop(0) must beEqualTo(actualStream)
    }
  }

  "drop n elements of stream" should {
    "take n elements from the head of the stream" in {
      val actualStream = Cons(() ⇒ "A",  () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))
      val expectedStream = Cons(() ⇒ "C", () ⇒ Empty)
      actualStream.drop(2).toList must beEqualTo(expectedStream.toList)
    }
  }
}
