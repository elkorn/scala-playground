package com.scalaz.day1;

object Main extends App {
 override def main(args: Array[String]) =  {
   println((
     Equal.show()::
     Order.show()::
     Show.show()::
     Enum.show()::
     Bounded.show()::
     Typeclasses.Step1.show()::
     Typeclasses.Step2.show()::
     YesNoTypeclass.show()::
     Nil)
     .mkString("\n"))
 }
}
