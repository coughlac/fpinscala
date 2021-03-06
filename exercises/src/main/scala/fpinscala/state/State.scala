package fpinscala.state

import scala.annotation.tailrec
import scala.math.BigDecimal.RoundingMode

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def doubleAlt(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(i ⇒ {val x = convertAndRound(i); x match { case 1 ⇒ convertAndRound(0); case _ ⇒ x}}).apply(rng)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (n, state) if n == Int.MinValue ⇒ (Int.MaxValue, state)
    case (n, state)                      ⇒ (n.abs, state)
  }

  def double(rng: RNG): (Double, RNG) = {
    nonNegativeInt(rng) match {
      case (n, state) if n == Int.MaxValue ⇒ (convertAndRound(0), state)
      case (n, state)                      ⇒ (convertAndRound(n), state)
    }
  }

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (intValue, intRNG) = rng.nextInt
    val (doubleValue, doubleRNG) = double(intRNG)
    ((intValue, doubleValue), doubleRNG)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (result, resRNG) = intDouble(rng)
    (result.swap, resRNG)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (double1Value, double1RNG) = double(rng)
    val (double2Value, double2RNG) = double(double1RNG)
    val (double3Value, double3RNG) = double(double2RNG)
    ((double1Value, double2Value, double3Value), double3RNG)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(acc :List[Int], count: Int, rng: RNG): (List[Int], RNG) = {
      count match {
        case 0 ⇒ (acc, rng)
        case _ ⇒
          val (value, nextRng) = rng.nextInt
          loop(value::acc, count-1, nextRng)
      }
    }
    loop(Nil, count, rng)
  }

  def intsSeq(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      val values = fs.map((elem: Rand[A]) => {
        val (value, _) = elem(rng)
        value
      })
      (values, rng)
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

  private def convertAndRound(n: Int): Double = BigDecimal.apply(n.toDouble / Int.MaxValue.toDouble).setScale(2, RoundingMode.UP).toDouble
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
