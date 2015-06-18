package gettingstarted

import fpinscala.laziness.{Cons, Empty, Stream}

class LazyStreamTest extends org.specs2.mutable.Specification {
  "toList" should {
    "return Nil if stream is empty" in {
      Empty.toList should beEqualTo(Nil)
    }
    "return the elements of the stream as a list if not empty" in {
      val actualStream = Cons(() ⇒ "A", () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))
      val expectedList = "A" :: "B" :: "C" :: Nil

      actualStream.toList should beEqualTo(expectedList)
    }
  }

  "take" should {
    "not change the original stream if it is empty" in {
      Empty.take(3) must beEqualTo(Empty)
    }

    "return an Empty Stream if n was 0" in {
      val actualStream = Cons(() ⇒ "A", () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))

      actualStream.take(0) must beEqualTo(Empty)
    }

    "take n elements from the head of the stream" in {
      val actualStream = Cons(() ⇒ "A", () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))
      val expectedStream = Cons(() ⇒ "A", () ⇒ Cons(() ⇒ "B", () ⇒ Empty))

      actualStream.take(2).toList must beEqualTo(expectedStream.toList)
    }

    "return the original stream if n is greater than the size of the original stream" in {
      val actualStream = Cons(() ⇒ "A", () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))
      val expectedStream = actualStream

      actualStream.take(4).toList must beEqualTo(expectedStream.toList)
    }
  }

  "takeAlt implemented using unfold" should {
    "not change the original stream if it is empty" in {
      Empty.takeAlt(3) must beEqualTo(Empty)
    }

    "return an Empty Stream if n was 0" in {
      val actualStream = Cons(() ⇒ "A", () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))

      actualStream.takeAlt(0) must beEqualTo(Empty)
    }

    "take n elements from the head of the stream" in {
      val actualStream = Cons(() ⇒ "A", () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))
      val expectedStream = Cons(() ⇒ "A", () ⇒ Cons(() ⇒ "B", () ⇒ Empty))

      actualStream.takeAlt(2).toList must beEqualTo(expectedStream.toList)
    }

    "return the original stream if n is greater than the size of the original stream" in {
      val actualStream = Cons(() ⇒ "A", () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))
      val expectedStream = actualStream

      actualStream.takeAlt(4).toList must beEqualTo(expectedStream.toList)
    }
  }

  "drop" should {
    "not change the original stream if it was an empty stream" in {
      Empty.drop(3) must beEqualTo(Empty)
    }

    "not change the original stream if n was 0" in {
      val actualStream = Cons(() ⇒ "A", () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))

      actualStream.drop(0) must beEqualTo(actualStream)
    }

    "take n elements from the head of the stream" in {
      val actualStream = Cons(() ⇒ "A", () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))
      val expectedStream = Cons(() ⇒ "C", () ⇒ Empty)

      actualStream.drop(2).toList must beEqualTo(expectedStream.toList)
    }
  }

  "takeWhile" should {
    "not change the original stream if it is empty" in {
      Empty.takeWhile(_.equals("A")) must beEqualTo(Empty)
    }

    "return an Empty Stream if predicate was never met" in {
      val actualStream = Cons(() ⇒ "A", () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))

      actualStream.takeWhile(_.equals(1)) must beEqualTo(Empty)
    }

    "returns elements of a Stream just until the predicate fails" in {
      val actualStream = Cons(() ⇒ "A", () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Cons(() ⇒ "D", () ⇒ Empty))))
      val expectedStream = Cons(() ⇒ "A", () ⇒ Cons(() ⇒ "B", () ⇒ Empty))

      actualStream.takeWhile(!_.equals("C")).toList must beEqualTo(expectedStream.toList)
    }
  }

  "takeWhileAlt using unfold" should {
    "not change the original stream if it is empty" in {
      Empty.takeWhileAlt(_.equals("A")) must beEqualTo(Empty)
    }

    "return an Empty Stream if predicate was never met" in {
      val actualStream = Cons(() ⇒ "A", () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Empty)))

      actualStream.takeWhileAlt(_.equals(1)) must beEqualTo(Empty)
    }

    "returns elements of a Stream just until the predicate fails" in {
      val actualStream = Cons(() ⇒ "A", () ⇒ Cons(() ⇒ "B", () ⇒ Cons(() ⇒ "C", () ⇒ Cons(() ⇒ "D", () ⇒ Empty))))
      val expectedStream = Cons(() ⇒ "A", () ⇒ Cons(() ⇒ "B", () ⇒ Empty))

      actualStream.takeWhileAlt(!_.equals("C")).toList must beEqualTo(expectedStream.toList)
    }
  }

  "forAll" should {
    "returns true if the stream is empty" in {
      Empty.forAll(_.equals("C")) must beTrue
    }

    "returns false if all the elements of a Stream do not pass the predicate" in {
      val actualStream = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 4, () ⇒ Empty))))
      actualStream.forAll(_ != 2) must beFalse
    }

    "returns true if all the elements of a Stream pass the predicate" in {
      val actualStream = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 4, () ⇒ Empty))))

      actualStream.forAll(_ < 5) must beTrue
    }
  }

  "headOption" should {
    "returns Some[A] if the stream is not empty" in {
      val actualStream = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 4, () ⇒ Empty))))

      actualStream.headOption must beEqualTo(Some(1))
    }

    "returns None if the stream is empty" in {
      Empty.headOption must beEqualTo(None)
    }
  }

  "map" should {
    "return an empty stream if the original stream was empty" in {
      val originalStream = Empty
      originalStream.map("hello") must beEqualTo(Empty)
    }

    "return a stream of all the elements from the original stream with f applied to each" in {
      val originalStream = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 4, () ⇒ Empty))))
      val expectedStream = Cons(() ⇒ 10, () ⇒ Cons(() ⇒ 20, () ⇒ Cons(() ⇒ 30, () ⇒ Cons(() ⇒ 40, () ⇒ Empty))))

      originalStream.map(_ * 10).toList must beEqualTo(expectedStream.toList)
    }
  }

  "mapAlt implemented with unfold" should {
    "return an empty stream if the original stream was empty" in {
      val originalStream = Empty
      originalStream.mapAlt(_ ⇒ "hello") must beEqualTo(Empty)
    }
    "return a stream of all the elements from the original stream with f applied to each" in {
      val originalStream = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 4, () ⇒ Empty))))
      val expectedStream = Cons(() ⇒ 10, () ⇒ Cons(() ⇒ 20, () ⇒ Cons(() ⇒ 30, () ⇒ Cons(() ⇒ 40, () ⇒ Empty))))
      originalStream.mapAlt(_ * 10).toList must beEqualTo(expectedStream.toList)
    }
  }

  "filter" should {
    "remove all odd numbers in list as predicate function filters for even elements" in {
      val originalStream = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 4, () ⇒ Empty))))
      val expectedStream = Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 4, () ⇒ Empty))

      originalStream.filter(x => x % 2 == 0).toList must beEqualTo(expectedStream.toList)
    }
  }

  "append" should {
    "leave the original stream unchanged if the stream to be appended is empty" in {
      val originalStream = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 4, () ⇒ Empty))))
      val additionalStream = Empty
      val expectedStream = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 4, () ⇒ Empty))))

      originalStream.append(additionalStream).toList must beEqualTo(expectedStream.toList)
    }

    "add the contents of the stream argument to the original stream u" in {
      val originalStream = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 4, () ⇒ Empty))))
      val additionalStream = Cons(() ⇒ 10, () ⇒ Cons(() ⇒ 20, () ⇒ Cons(() ⇒ 30, () ⇒ Cons(() ⇒ 40, () ⇒ Empty))))
      val expectedStream = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 4,
        () ⇒ Cons(() ⇒ 10, () ⇒ Cons(() ⇒ 20, () ⇒ Cons(() ⇒ 30, () ⇒ Cons(() ⇒ 40, () ⇒ Empty))))))))

      originalStream.append(additionalStream).toList must beEqualTo(expectedStream.toList)
    }
  }

  "flatMap" should {
    "return a stream with f applied to each element in the original stream and flattened" in {
      val originalStream = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 4, () ⇒ Empty))))
      val expectedStream = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 4, () ⇒ Empty))))

      originalStream.flatMap(i => Cons(() => i, () => Empty)).toList must beEqualTo(expectedStream.toList)
    }
  }

  "constant" should {
    "return an infinite stream with a" in {
      Stream.constant(1).take(100).toList must beEqualTo(Stream.ones.take(100).toList)
    }
  }

  "from" should {
    "return an infinite stream with a" in {
      val expectedStream = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 4, () ⇒ Empty))))

      Stream.from(1).take(4).toList must beEqualTo(expectedStream.toList)
    }
  }

  "fibs" should {
    "return an infinite stream of fibonacci numbers 0, 1, 1, 2, 3, 5, 8 etc" in {
      val expectedStream = Cons(() ⇒ 0, () ⇒ Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 5, () ⇒ Cons(() ⇒ 8, () ⇒ Empty)))))))

      Stream.fibs(0).take(7).toList must beEqualTo(expectedStream.toList)
    }
  }

  "unfold" should {
    "take an initial state an a function for producing both the next state and the next value in the generated stream. " +
      "Option is used to indicate when the Stream should be terminated, if at all." in {
      val expectedStream = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 4, () ⇒ Empty))))
      Stream.unfold(0)({ case 4 ⇒ None case x ⇒ Some(x + 1, x + 1) }).toList must beEqualTo(expectedStream.toList)
    }

    "as an implementation of the ones function" in {
      Stream.onesAlt.take(6).toList must beEqualTo(Stream.ones.take(6).toList)
    }

    "as an implementation of the constant function" in {
      Stream.constantAlt(9).take(6).toList must beEqualTo(Stream.constant(9).take(6).toList)
    }

    "as an implementation of the from function" in {
      Stream.fromAlt(1).take(4).toList must beEqualTo(Stream.from(1).take(4).toList)
    }

    "as an implementation of the fibonacci function" in {
      Stream.fibsAlt(0).take(7).toList must beEqualTo(Stream.fibs(0).take(7).toList)
    }
  }

  "zipWith using unfold" should {
    "add corresponding elements of the streams together and return the output stream" in {
      val stream1 = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Empty)))
      val stream2 = Cons(() ⇒ 4, () ⇒ Cons(() ⇒ 5, () ⇒ Cons(() ⇒ 6, () ⇒ Empty)))

      val result = Stream.zipWith(stream1, stream2)((x, y) => x + y)

      result.toList must beEqualTo(List(5, 7, 9))
    }
  }

  "zipAll using unfold" should {
    "interleave corresponding elements of the streams together" in {
      val stream1 = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Empty)))
      val stream2 = () ⇒ Empty

      stream1.zipAll(stream2()).toList must beEqualTo(List((Some(1), None), (Some(2), None), (Some(3), None)))
      stream2().zipAll(stream1).toList must beEqualTo(List((None, Some(1)), (None, Some(2)), (None, Some(3))))
    }

    "interleave corresponding elements of the streams together" in {
      val stream1 = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Empty)))
      val stream2 = Cons(() ⇒ 4, () ⇒ Cons(() ⇒ 5, () ⇒ Cons(() ⇒ 6, () ⇒ Empty)))

      stream1.zipAll(stream2).toList must beEqualTo(List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), Some(6))))
    }

    "interleave corresponding elements of the streams together even if not the same length" in {
      val stream1 = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Empty)))
      val stream2 = Cons(() ⇒ 5, () ⇒ Cons(() ⇒ 6, () ⇒ Empty))

      stream1.zipAll(stream2).toList must beEqualTo(List((Some(1), Some(5)), (Some(2), Some(6)), (Some(3), None)))
      stream2.zipAll(stream1).toList must beEqualTo(List((Some(5), Some(1)), (Some(6), Some(2)), (None, Some(3))))
    }
  }

  "startsWith" should {
    "be true if the stream starts with all the same elements in the same order" in {
      val stream1 = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Empty)))
      val stream2 = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Empty)))
      stream1.startsWith(stream2) should beTrue
    }

    "be false if the comparison stream is empty" in {
      val stream1 = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Empty)))
      val stream2 = () ⇒ Empty
      stream1.startsWith(stream2()) should beFalse
    }

    "be false if the elements at the start of the stream do not match (even if a subset)" in {
      val stream1 = Cons(() ⇒ 5, () ⇒ Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Empty))))
      val stream2 = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Empty)))
      stream1.startsWith(stream2) should beFalse
    }

    "be false if the order is not the same" in {
      val stream1 = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 4, () ⇒ Empty))))
      val stream2 = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 2, () ⇒ Empty)))
      val stream3 = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 4, () ⇒ Empty)))
      stream1.startsWith(stream2) should beFalse
      stream1.startsWith(stream3) should beFalse
    }

    "be false if all the elements of the comparison stream are not there" in {
      val stream1 = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 3, () ⇒ Cons(() ⇒ 4, () ⇒ Empty))))
      val stream2 = Cons(() ⇒ 1, () ⇒ Cons(() ⇒ 2, () ⇒ Cons(() ⇒ 5, () ⇒ Empty)))
      stream1.startsWith(stream2) should beFalse
    }
  }
}
