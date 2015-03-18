package com.scalaz.day2;

object Main extends App {
 override def main(args: Array[String]) =  {
   println((
     FunctorTypeclass.show()::
     ApplicativeTypeclass.show()::
     ApplicativeTypeclass.ApplyTypeclass.show()::
     Nil)
     .mkString("\n"))
 }
}
