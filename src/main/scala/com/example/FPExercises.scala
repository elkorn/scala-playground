package com.example

import scala.annotation.tailrec

/**
 * Created by elkorn on 12/5/14.
 */
object FPExercises {
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 1) acc
      else if (n == 2) acc + 1
      else {
        go(n - 1, acc + fib(n - 2))
      }
    }

    go(n, 0)
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 1) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }
}
