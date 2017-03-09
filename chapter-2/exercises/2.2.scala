object Exercise {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def go(index: Int): Boolean = {
      if (index >= as.length) true
      else if (!ordered(as(index - 1), as(index))) false
      else go(index + 1)
    }

    go(1)
  }
}
