package gettingstarted

import fpinscala.laziness.{Cons, Empty}

class LazyStreamTest extends org.specs2.mutable.Specification {
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

    "return an Empty Stream if n was 0" in {
      val actualStream = Cons(() ⇒ "A",  () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))
      actualStream.take(0) must beEqualTo(Empty)
    }

    "take n elements from the head of the stream" in {
      val actualStream = Cons(() ⇒ "A",  () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))
      val expectedStream = Cons(() ⇒ "A",  () ⇒ Cons(() ⇒ "B", ()⇒ Empty))
      actualStream.take(2).toList must beEqualTo(expectedStream.toList)
    }

    "return the original stream if n is greater than the size of the original stream" in {
      val actualStream = Cons(() ⇒ "A",  () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))
      val expectedStream = actualStream
      actualStream.take(4).toList must beEqualTo(expectedStream.toList)
    }
  }

  "drop" should {
    "not change the original stream if it was an empty stream" in {
      Empty.drop(3) must beEqualTo(Empty)
    }

    "not change the original stream if n was 0" in {
      val actualStream = Cons(() ⇒ "A",  () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))
      actualStream.drop(0) must beEqualTo(actualStream)
    }

    "take n elements from the head of the stream" in {
      val actualStream = Cons(() ⇒ "A",  () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))
      val expectedStream = Cons(() ⇒ "C", () ⇒ Empty)
      actualStream.drop(2).toList must beEqualTo(expectedStream.toList)
    }
  }
}
