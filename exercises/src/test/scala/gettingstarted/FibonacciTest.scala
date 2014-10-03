package gettingstarted

import fpinscala.gettingstarted.MyModule.fib

class FibonacciTest extends org.specs2.mutable.Specification {

  "A fibonacci sequence" should {
    "be correct" in {
      fib(0) must beEqualTo(0)
      fib(1) must beEqualTo(1)
      fib(2) must beEqualTo(1)
      fib(3) must beEqualTo(2)
      fib(4) must beEqualTo(3)
      fib(5) must beEqualTo(5)
      fib(6) must beEqualTo(8)
      fib(7) must beEqualTo(13)
      fib(8) must beEqualTo(21)
    }
  }
}
