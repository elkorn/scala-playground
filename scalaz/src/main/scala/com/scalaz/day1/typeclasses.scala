package com.scalaz.day1;

import scalaz._;
import Scalaz._;

object Typeclasses {
  object Step1 {
    // Equivalent of data TrafficLight = red | green | yellow
    sealed trait TrafficLight
    case object Red extends TrafficLight 
    case object Yellow extends TrafficLight 
    case object Green extends TrafficLight 

    // Trying to define a ScalaZ Equal for traffic lights.
    implicit val TrafficLightEqual: Equal[TrafficLight] = scalaz.Equal.equal(_ == _)

    // Doesn't work due to nonvariant subtyping of Equal[F].
    def show() = (
      // "Typeclasses",
      // Red === Yellow
      )
  }

  object Step2 {
    case class TrafficLight(name: String)
    val red = TrafficLight("red")
    val yellow = TrafficLight("yellow")
    val green = TrafficLight("green")
    implicit val TrafficLightEqual: Equal[TrafficLight] = scalaz.Equal.equal(_ == _)

    def show() = (
      "Typeclasses2",
      red === yellow,
      red === red
      )

  }
}
