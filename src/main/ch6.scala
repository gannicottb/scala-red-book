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
//    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
//      rng => {
//        val (a, rng2) = s(rng)
//        (f(a), rng2)
//      }

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

    def nonNegativeInt: Rand[Int] = rng => {
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

//    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
//      val (a, next)  = ra(rng)
//      val (b, next2) = rb(next)
//      (f(a, b), next2)
//    }
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      flatMap(ra)(a => map(rb)(b => f(a, b)))
    }

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
      map2(ra, rb)((_, _))

    def nonNegativeEven: Rand[Int] =
      map(nonNegativeInt)(i => i - 1 % 2)

    def double: Rand[Double] = map(nonNegativeInt)(i => i / Int.MaxValue.toDouble + 1)

    def randIntDouble: Rand[(Int, Double)] = both(int, double)
    def randDoubleInt: Rand[(Double, Int)] = both(double, int)

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
      @tailrec
      def go(transitions: List[Rand[A]], nextRng: RNG, results: List[A]): (List[A], RNG) = transitions match {
        case h :: tail =>
          val (g, r) = h(nextRng)
          go(tail, r, results :+ g)
        case _ => (results, nextRng)
      }
      go(fs, rng, List())
    }

    def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }

    def nonNegativeLessThan(n: Int): Rand[Int] =
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0)
          unit(mod)
        else
          nonNegativeLessThan(n)
      }
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n       = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }
//  def double(rng: RNG): (Double, RNG) = {
//    val (i, nextRng) = nonNegativeInt(rng)
//    val d            = s"0.$i".toDouble
//    (d, nextRng)
//  }
//  def double(rng: RNG): (Double, RNG) = {
//    val (i, nextRng) = nonNegativeInt(rng)
//    val d            = (i / Int.MaxValue.toDouble + 1)
//    (d, nextRng)
//  }
//
//  def intDouble(rng: RNG): ((Int, Double), RNG) = {
//    val (i, nextRNG)  = rng.nextInt
//    val (d, nextRNG2) = double(nextRNG)
//    ((i, d), nextRNG2)
//  }
//
//  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
//    val (d, nextRNG)  = double(rng)
//    val (i, nextRNG2) = rng.nextInt
//    ((d, i), nextRNG2)
//  }
//
//  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
//    val (d1, nextRNG1) = double(rng)
//    val (d2, nextRNG2) = double(nextRNG1)
//    val (d3, nextRNG3) = double(nextRNG2)
//    ((d1, d2, d3), nextRNG3)
//  }
//
//  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
//    @tailrec
//    def go(list: List[Int], nextRng: RNG, max: Int): (List[Int], RNG) = max match {
//      case 0 => (list, nextRng)
//      case _ =>
//        val (i, next) = nextRng.nextInt
//        go(list :+ i, next, max - 1)
//    }
//
//    go(List[Int](), rng, count)
//  }

  case class State[S, +A](run: S => (A, S)) {

    /**
      *  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }
      */
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s => {
        val (a, ns) = run(s)
        f(a).run(ns)
      })
    def map[B](f: A => B): State[S, B]                           = flatMap(a => State.unit(f(a)))
    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))
    def sequence[S, A](ts: List[State[S, A]]): State[S, List[A]] = {
      ts.foldRight(unit[S, List[A]](List[A]())) {
        case (b, memo) => b.map2(memo)(_ :: _)
      }
    }
  }

}
