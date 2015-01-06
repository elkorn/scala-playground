package com.fpinscala.testing

case class SGen[+A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] =
    SGen(forSize andThen (_ map f))

  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(forSize andThen (_ flatMap f))

  def apply(n: Int): Gen[A] = forSize(n)

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  def zipWith[B](other: Gen[B]): SGen[(A, B)] =
    SGen(n => forSize(n).zipWith(other))

  def zipWith[B](other: SGen[B]): SGen[(A, B)] =
    SGen(n => forSize(n).zipWith(other.forSize(n)))
}
