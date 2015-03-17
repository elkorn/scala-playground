package com.scalaz.day1;

object Main extends App {
 override def main(args: Array[String]) =  {
   println((
     Equal.show()::
     Order.show()::
     Show.show()::
     Nil)
     .mkString("\n"))
 }
}
