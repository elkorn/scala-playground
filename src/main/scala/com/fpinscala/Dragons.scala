package com.fpinscala

import java.util.concurrent.TimeUnit

import scala.concurrent.Future
import scala.concurrent.duration.Duration

object Dragons {
  def wrapJavaFutureInScalaFuture[T](javaFuture: java.util.concurrent.Future[T], maybeTimeout: Option[Duration] = None): scala.concurrent.Future[T] = {
    Future {
      // Fuggetabout that thread...
      maybeTimeout match {
        case None => javaFuture.get
        case Some(d) => javaFuture.get(d.toMillis, TimeUnit.MILLISECONDS)
      }
    }
  }
}
