package main

import main.ch6.SimpleRNG

object Main {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(9320839213.0.toLong)

    val (n, nextRng) = ch6.nonNegativeInt(rng)
    println(s"NonNegative: $n")

    val (d, nextRng2) = ch6.double(nextRng)
    println(s"Double: $d")

    val (is, _) = ch6.ints(10)(nextRng2)
    println(s"Ints: $is")
  }
}
