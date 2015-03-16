package com.scalaz.day0;

object Main extends App {
 override def main(args: Array[String]) =  {
   (Sum.Monoid.Step4.Step4_2.showDefault()::
   Sum.Monoid.Step4.Step4_2.showSum()::
   FoldLeftTypeclass.FoldLeftList.show()::
   FoldLeftTypeclass.Generic.show()::
   Nil).map(println)
 }
}
