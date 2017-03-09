object Exercise {
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
}
