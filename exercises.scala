object ExercisesModule {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, a: Int, b: Int): Int = {
      if (n <= 0) a
      else go(n - 1, b, a + b)
    }
    go(n, 0, 1)
  }

  def isSorted[A] (as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length < 2) return true // sorted by definition
    def go(index: Int, sorted: Boolean): Boolean = {
      if (!sorted || index >= as.length - 1) sorted
      else go(index + 1, ordered(as(index), as(index + 1)))
    }
    go(0, sorted = true)
  }
  def main(args: Array[String]): Unit = {
    // 2.1
    0.to(end = 10) foreach (i => println("%s: %s".format(i, fib(i))))

    // 2.2
    for {
      i <- Array(
        Array(1),
        Array(1,2),
        Array(1,2,3),
        Array(3,2,1)
      )
    } println(isSorted(i, (x: Int, y: Int) => x <= y))
  }
}
