package main

import main.ch6.{RNG, SimpleRNG}

object Main {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(9320839213.0.toLong)

    RNG
      .sequence(
        List(
          RNG.nonNegativeInt,
          RNG.ints(10)(_),
          RNG.double,
          RNG.double,
          RNG.int,
          RNG.nonNegativeLessThan(100)
        )
      )(rng)
      ._1
      .map(println)
  }
}
