object Exercises {
  // 2.1
  // Any fibonnaci number above 47 will be incorrect since it begins wrapping the integer
  def fib(n: Int): Int = {
    def go(first: Int, second: Int, on: Int): Int = {
      if (n <= on)
        second
      else
        go(second, first + second, on + 1)
    }

    if (n <= 1)
      0
    else
      go(0, 1, 2)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def go(index: Int): Boolean = {
      if (index >= as.length) true
      else if (!ordered(as(index - 1), as(index))) false
      else go(index + 1)
    }

    go(1)
  }

  // 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a) => f(a, _)
  }

  // 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a) => f(g(a))
  }
}
