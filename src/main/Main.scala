package main

import main.ch6.{Coin, Machine, RNG, SimpleRNG, State, Turn}

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

    val r =
      State
        .simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
        .run(Machine(locked = true, coins = 10, candies = 5))
    println(s"SimulateMachine result: ${r._1}")
  }
}
