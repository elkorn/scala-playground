package com.scalaz.day1;

import scalaz._;
import Scalaz._;

object YesNoTypeclass {
  // Naming convention borrowed from CanBuildFrom, but with
  // truthiness as the operation.
  trait CanTruthy[A] { self => 
    def truthys(a: A): Boolean
  }

  object CanTruthy {
    def apply[A](implicit ev: CanTruthy[A]): CanTruthy[A] = ev
    def truthys[A](f: A=>Boolean): CanTruthy[A] = new CanTruthy[A] {
      def truthys(a: A): Boolean = f(a)
    }
  }

  import scala.language.implicitConversions;
  trait CanTruthyOps[A] {
    def self: A
    implicit def F: CanTruthy[A]
    final def truthy: Boolean = F.truthys(self)
  }
  object ToCanTruthyOps {
    implicit def toCanTruthyOps[A](v: A)(implicit ev: CanTruthy[A]) =
      new CanTruthyOps[A] {
        def self = v
        implicit def F: CanTruthy[A] = ev
      }
  }

  import ToCanTruthyOps._


  implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.truthys({
    case 0 => false
    case _ => true
  })

  implicit def stringCanTruthy: CanTruthy[String] = 
    CanTruthy.truthys({
      case null => false
      case "" => false
      case _ => true
    })

  implicit def listCanTruthy[A]: CanTruthy[List[A]] = 
    CanTruthy.truthys({
      case Nil => false
      case _ => true
    })

  // Nil is nonvariant and needs to be treated separately
  // from a polymorfic list
  implicit def nilCanTruthy: CanTruthy[Nil.type] = 
    CanTruthy.truthys(_ => false)

  // Using an id function from ScalaZ.
  implicit def boolIsTruthy: CanTruthy[Boolean] = 
    CanTruthy.truthys(identity)

  // functional if statement
  // delayed result blocks execution - only one branch is exec'd.
  def truthyIf[A : CanTruthy, B, C](cond: A)(yes: => B)(no: => C) = 
    if(cond.truthy) yes
    else no

  def truthyTest[A : CanTruthy](v: A) =
    truthyIf(v){s"$v is truthy"}{s"$v is not truthy"}

  def show() = (
    "YesNoTypeclass",
    "".truthy,
    "aaa".truthy,
    12.truthy,
    0.truthy,
    List(1,2,3).truthy,
    Nil.truthy,
    true.truthy,
    false.truthy,
    truthyTest(Nil),
    truthyTest(1),
    truthyTest(true),
    truthyTest("aaa")
    )
}
