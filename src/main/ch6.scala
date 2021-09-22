package main

import scala.annotation.tailrec

object ch6 {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  type Rand[+A] = RNG => (A, RNG)

  object RNG {
    val int: Rand[Int]         = _.nextInt
    def unit[A](a: A): Rand[A] = rng => (a, rng)
    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
      val (a, next)  = ra(rng)
      val (b, next2) = rb(next)
      (f(a, b), next2)
    }

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
      map2(ra, rb)((_, _))

    def nonNegativeEven: Rand[Int] =
      map(nonNegativeInt)(i => i - 1 % 2)

    def double: Rand[Double] = map(nonNegativeInt)(i => i / Int.MaxValue.toDouble + 1)

  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n       = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nextRng) = rng.nextInt
    val nonNeg = if (i == Int.MinValue) {
      -(i + 1)
    } else if (i < 0) {
      -i
    } else {
      i
    }
    (nonNeg, nextRng)
  }

//  def double(rng: RNG): (Double, RNG) = {
//    val (i, nextRng) = nonNegativeInt(rng)
//    val d            = s"0.$i".toDouble
//    (d, nextRng)
//  }
  def double(rng: RNG): (Double, RNG) = {
    val (i, nextRng) = nonNegativeInt(rng)
    val d            = (i / Int.MaxValue.toDouble + 1)
    (d, nextRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, nextRNG)  = rng.nextInt
    val (d, nextRNG2) = double(nextRNG)
    ((i, d), nextRNG2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, nextRNG)  = double(rng)
    val (i, nextRNG2) = rng.nextInt
    ((d, i), nextRNG2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, nextRNG1) = double(rng)
    val (d2, nextRNG2) = double(nextRNG1)
    val (d3, nextRNG3) = double(nextRNG2)
    ((d1, d2, d3), nextRNG3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(list: List[Int], nextRng: RNG, max: Int): (List[Int], RNG) = max match {
      case 0 => (list, nextRng)
      case _ =>
        val (i, next) = nextRng.nextInt
        go(list :+ i, next, max - 1)
    }

    go(List[Int](), rng, count)
  }
}
