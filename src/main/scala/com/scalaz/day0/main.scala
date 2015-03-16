package com.scalaz.day0;

object Main extends App {
 override def main(args: Array[String]) =  {
   println((
     Sum.Monoid.Step4.Step4_2.showDefault()::
     Sum.Monoid.Step4.Step4_2.showSum()::
     FoldLeftTypeclass.FoldLeftList.show()::
     FoldLeftTypeclass.Generic.show()::
     MethodInjection.Bare.show()::
     MethodInjection.Scalaz7.show()::
     Nil)
     .mkString("\n"))
 }
}
