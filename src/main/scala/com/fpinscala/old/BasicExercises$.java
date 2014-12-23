package com.fpinscala.old;

object BasicExercises {
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

        def indexOf[T](source: Array[T], predicate: T => Boolean): Int = {
                @tailrec
                        def go(n: Int): Int = {
                        if (n >= source.length) -1
                                else if (predicate(source(n))) n
                                else go(n + 1)
                        }

                go(0)
                }

        def isSorted[T](source: Array[T], isSorted: (T, T) => Boolean): Boolean = {
                @tailrec
                        def go(n: Int): Boolean = {
                        if (n >= source.length - 1) true
                                else if (!isSorted(source(n), source(n + 1))) false
                                else go(n + 1)
                        }

                go(0)
                }

        def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
                (b: B) => f(a, b)

        def curry[A, B, C](f: (A, B) => C): A => B => C =
                (a: A) => (b: B) => f(a, b)

        def uncurry[A, B, C](f: A => B => C): (A, B) => C =
                (a: A, b: B) => f(a)(b)
        }
