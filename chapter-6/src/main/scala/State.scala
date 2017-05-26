package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
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

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, next) = rng.nextInt
    if (int < 0) {
      (-(int + 1), next)
    } else {
      (int, next)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (int, next) = nonNegativeInt(rng)
    (int / (Int.MaxValue + 1).toDouble, next)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, next1) = rng.nextInt
    val (doub, next2) = double(next1)
    ((int, doub), next2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (doub, next1) = double(rng)
    val (int, next2) = next1.nextInt
    ((doub, int), next2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (double1, next1) = double(rng)
    val (double2, next2) = double(next1)
    val (double3, next3) = double(next2)
    ((double1, double2, double3), next3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(next: RNG, count: Int, l: List[Int]): (List[Int], RNG) = {
      if (count == 0) {
        (l, next)
      } else {
        val (int, next2) = next.nextInt
        go(next2, count - 1, int :: l)
      }

    }

    go(rng, count, List())
  }

  val doubleViaMap: Rand[Double] =
    map(nonNegativeInt)((_ / (Int.MaxValue + 1).toDouble))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, next) = ra(rng)
      val (b, next2) = rb(next)
      (f(a,b), next2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)
    }
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))
}

case class State[S,+A](run: S => (A, S)) {
  import State._
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State({ s =>
            val (a, s2) = run(s)
            f(a).run(s2)
          })
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[S,A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S,A](fs: List[State[S,A]]): State[S, List[A]] =
    fs.foldRight(unit[S,List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(m =>
    {
      val finalMachine = inputs.foldLeft(m) { (machine, input) =>
        val candiesLeft = machine.candies
        val coinsContained = machine.coins
        input match {
          case Coin if machine.locked && candiesLeft > 0 => Machine(false, candiesLeft, coinsContained + 1)
          case Turn if !machine.locked => Machine(true, candiesLeft - 1, coinsContained)
          case _ => machine
        }
      }

      ((finalMachine.coins,finalMachine.candies), finalMachine)
    }
  )
}
